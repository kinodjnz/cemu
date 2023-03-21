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
        fcntl::fcntl(stdin_fd, fcntl::FcntlArg::F_SETFL(fcntl::OFlag::O_NONBLOCK))?;
        let termios = Termios::from_fd(stdin_fd).unwrap();
        let mut new_termios = termios.clone();
        new_termios.c_lflag &= !(termios::ICANON | termios::ECHO);
        termios::tcsetattr(stdin_fd, termios::TCSANOW, &mut new_termios).unwrap();

        Ok(Uart {
            stdin,
            input: Option::None,
            step_count: INTERRUPT_INTERVAL,
        })
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
                let mut buf = [0; 1];
                if let Ok(_) = self.stdin.read_exact(&mut buf) {
                    self.input = Some(buf[0]);
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
