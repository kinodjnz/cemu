use crate::cpu::{Cpu, IntoWord, MemoryMappedIo, Word};

pub struct InterruptController {}

impl InterruptController {
    pub fn new() -> InterruptController {
        InterruptController {}
    }
}

impl MemoryMappedIo for InterruptController {
    fn read(&self, _addr: Word, _cpu: &Cpu) -> Word {
        0xdeadbeefu32.into_word()
    }

    fn write(&mut self, _addr: Word, _data: Word, _cpu: &Cpu) {}
}
