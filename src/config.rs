use crate::cpu::{Cpu, IntoWord, MemoryMappedIo, Word};

pub struct CpuConfig;

impl CpuConfig {
    const CLOCK: u32 = 100038910 / 10; // for emulation, 1/10 of actual clock
    pub fn new() -> CpuConfig {
        CpuConfig {}
    }
}

impl MemoryMappedIo for CpuConfig {
    fn read(&mut self, addr: Word, _cpu: &Cpu) -> Word {
        match (addr.0 >> 2) & 1 {
            0 => 0x01234567.into_word(),
            _ => Self::CLOCK.into_word(),
        }
    }

    fn write(&mut self, _addr: Word, _data: Word, _cpu: &Cpu) {}
}
