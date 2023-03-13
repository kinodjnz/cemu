mod cpu;

use cpu::*;

fn main() {
    let mut cpu = Cpu::new();
    cpu.ram[0] = 0x00044117u32.into_word();
    cpu.ram[1] = 0x7139a8e5u32.into_word();
    println!("{}", cpu);
    cpu.step();
    println!("{}", cpu);
    cpu.step();
    println!("{}", cpu);
}
