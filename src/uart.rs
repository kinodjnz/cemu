use crate::cpu::{Cpu, InterruptSource, IntoWord, MemoryMappedIo, Word};
use nix::fcntl;
use std::io;
use std::io::Error;
use std::io::Read;
use std::os::fd::AsRawFd;
use termios::Termios;

const INTERRUPT_INTERVAL: u32 = 256;

pub struct Uart {
    stdin: io::Stdin,
    input: Option<u8>,
    step_count: u32,
}

impl Uart {
    pub fn new() -> Result<Uart, Error> {
        let stdin = io::stdin();
        let stdin_fd = stdin.as_raw_fd();
        let termios = Termios::from_fd(stdin_fd).unwrap();
        let mut new_termios = termios;
        new_termios.c_lflag &= !(termios::ICANON | termios::ECHO);
        termios::tcsetattr(stdin_fd, termios::TCSANOW, &new_termios).unwrap();

        Ok(Uart {
            stdin,
            input: Option::None,
            step_count: INTERRUPT_INTERVAL,
        })
    }

    fn read_nonblocking(&mut self) -> Result<u8, Error> {
        let stdin_fd = self.stdin.as_raw_fd();
        let original_fl = fcntl::fcntl(stdin_fd, fcntl::FcntlArg::F_GETFL)?;
        let original_fl = fcntl::OFlag::from_bits_truncate(original_fl);
        let nonblocking = original_fl | fcntl::OFlag::O_NONBLOCK;
        fcntl::fcntl(stdin_fd, fcntl::FcntlArg::F_SETFL(nonblocking))?;
        let mut buf = [0; 1];
        let result = self.stdin.read_exact(&mut buf);
        fcntl::fcntl(stdin_fd, fcntl::FcntlArg::F_SETFL(original_fl))?;
        result.map(|_| buf[0])
    }
}

impl MemoryMappedIo for Uart {
    fn read(&mut self, addr: Word, _cpu: &Cpu) -> Word {
        match (addr.0 >> 2) & 1 {
            0 => (if self.input.is_some() { 8 } else { 0 }).into_word(),
            _ => (if let Some(c) = self.input {
                self.input = Option::None;
                c as u32
            } else {
                0xdeadbeefu32
            })
            .into_word(),
        }
    }

    fn write(&mut self, addr: Word, data: Word, _cpu: &Cpu) {
        match (addr.0 >> 2) & 1 {
            0 => (),
            _ => print!("{}", char::from_u32(data.0).unwrap_or(' ')),
        }
    }
}

impl InterruptSource for Uart {
    fn step_intr(&mut self, _cpu: &Cpu) -> bool {
        self.step_count -= 1;
        if self.step_count == 0 {
            self.step_count = INTERRUPT_INTERVAL;
            if self.input.is_some() {
                true
            } else {
                if let Ok(c) = self.read_nonblocking() {
                    self.input = Some(c);
                    true
                } else {
                    false
                }
            }
        } else {
            self.input.is_some()
        }
    }
}
