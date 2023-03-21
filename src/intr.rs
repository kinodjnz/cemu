use crate::cpu::{Cpu, InterruptSource, IntoWord, MemoryMappedIo, Word};
use std::cell::RefCell;
use std::rc::Rc;

pub struct InterruptController {
    sources: Vec<Rc<RefCell<dyn InterruptSource>>>,
}

impl InterruptController {
    pub fn new() -> InterruptController {
        InterruptController { sources: vec![] }
    }

    pub fn add_source(&mut self, source: Rc<RefCell<dyn InterruptSource>>) {
        self.sources.push(source);
    }
}

impl MemoryMappedIo for InterruptController {
    fn read(&mut self, _addr: Word, _cpu: &Cpu) -> Word {
        0xdeadbeefu32.into_word()
    }

    fn write(&mut self, _addr: Word, _data: Word, _cpu: &Cpu) {}
}

impl InterruptSource for InterruptController {
    fn step_intr(&mut self, cpu: &Cpu) -> bool {
        self.sources.iter().any(|s| s.borrow_mut().step_intr(cpu))
    }
}
