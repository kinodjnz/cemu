mod config;
mod cpu;
mod disasm;
mod intr;
mod uart;

use config::CpuConfig;
use cpu::*;
use disasm::disasm;
use intr::InterruptController;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io::Error;
use std::process::ExitCode;
use std::rc::Rc;
use uart::Uart;

fn main() -> Result<ExitCode, Error> {
    if env::args().len() < 2 {
        println!("usage: {} executable.bin", env::args().next().unwrap());
        return Result::Ok(ExitCode::from(1));
    }
    let bin: Vec<u8> = fs::read(env::args().nth(1).unwrap())?;
    let uart = Rc::new(RefCell::new(Uart::new()));
    let intr = Rc::new(RefCell::new(InterruptController::new()));
    let config = Rc::new(RefCell::new(CpuConfig::new()));
    let mut cpu = Cpu::new();
    cpu.add_memory_map(MemoryMap::new(0x30001000, 8, uart));
    cpu.add_memory_map(MemoryMap::new(0x30004000, 8, intr));
    cpu.add_memory_map(MemoryMap::new(0x40000000, 8, config));
    for i in (0..bin.len()).step_by(4) {
        cpu.ram[i / 4] = (bin[i] as u32).into_word()
            | (bin[i + 1] as u32).into_word() << 8
            | (bin[i + 2] as u32).into_word() << 16
            | (bin[i + 3] as u32).into_word() << 24;
    }
    for i in bin.len() & !3..bin.len() {
        cpu.ram[i / 4] = (bin[i] as u32).into_word() << ((i & 3) * 8)
    }
    println!("{}", cpu);
    while cpu.cycle < 0xffff_ffff && cpu.pc != 0x2fff_ffff.into_word() {
        cpu.step();
    }
    for _ in 0..500 {
        println!(
            "{:08x} {}",
            cpu.next_inst(),
            disasm(cpu.next_inst()).with_pc(cpu.pc)
        );
        cpu.step();
        println!("{}", cpu);
    }
    Ok(ExitCode::SUCCESS)
}
