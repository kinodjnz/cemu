use crate::cpu::{Cpu, InterruptSource, IntoWord, MemoryMappedIo, Word};
use std::cell::RefCell;
use std::rc::Rc;

pub struct InterruptController {
    sources: Vec<Rc<RefCell<dyn InterruptSource>>>,
    enabled: u32,
    asserted: u32,
}

impl InterruptController {
    pub fn new() -> InterruptController {
        InterruptController {
            sources: vec![],
            enabled: 0,
            asserted: 0,
        }
    }

    pub fn add_source(&mut self, source: Rc<RefCell<dyn InterruptSource>>) {
        self.sources.push(source);
    }
}

impl MemoryMappedIo for InterruptController {
    fn read(&mut self, addr: Word, _cpu: &Cpu) -> Word {
        match (addr.0 >> 2) & 1 {
            0 => self.enabled.into_word(),
            _ => self.asserted.into_word(),
        }
    }

    fn write(&mut self, _addr: Word, data: Word, _cpu: &Cpu) {
        self.enabled = data.0;
    }
}

impl InterruptSource for InterruptController {
    fn step_intr(&mut self, cpu: &Cpu) -> bool {
        self.asserted = self
            .sources
            .iter()
            .map(|s| s.borrow_mut().step_intr(cpu))
            .enumerate()
            .fold(0, |acc, (i, b)| {
                acc | if b { self.enabled & (1 << i) } else { 0 }
            });
        self.asserted != 0
    }
}
