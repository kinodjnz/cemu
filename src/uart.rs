use crate::cpu::{Cpu, IntoWord, MemoryMappedIo, Word};

pub struct Uart;

impl Uart {
    pub fn new() -> Uart {
        Uart {}
    }
}

impl MemoryMappedIo for Uart {
    fn read(&self, addr: Word, _cpu: &Cpu) -> Word {
        match (addr.0 >> 2) & 1 {
            0 => 0.into_word(),
            _ => 0xdeadbeefu32.into_word(),
        }
    }

    fn write(&mut self, addr: Word, data: Word, _cpu: &Cpu) {
        match (addr.0 >> 2) & 1 {
            0 => (),
            _ => print!("{}", char::from_u32(data.0).unwrap_or(' ')),
        }
    }
}
