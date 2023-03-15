mod cpu;
mod disasm;

use cpu::*;
use disasm::disasm;
use std::env;
use std::fs;
use std::io::Error;
use std::process::ExitCode;

fn main() -> Result<ExitCode, Error> {
    if env::args().len() < 2 {
        println!("usage: {} executable.bin", env::args().next().unwrap());
        return Result::Ok(ExitCode::from(1));
    }
    let bin: Vec<u8> = fs::read(env::args().nth(1).unwrap())?;
    let mut cpu = Cpu::new();
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
    while cpu.pc.0 != 0x200000d8 {
        cpu.step();
    }
    for _ in 0..40 {
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
