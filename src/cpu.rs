use std::fmt;
use std::num::Wrapping;

type Word = Wrapping<u32>;

pub trait IntoWord {
    fn into_word(self) -> Word;
}

impl IntoWord for u32 {
    fn into_word(self) -> Word {
        Wrapping(self)
    }
}

impl IntoWord for i32 {
    fn into_word(self) -> Word {
        Wrapping(self as u32)
    }
}

pub trait SignedLt {
    fn slt(&self, rhs: &Self) -> bool;
}

impl SignedLt for Word {
    fn slt(&self, rhs: &Self) -> bool {
        (self.0 as i32) < (rhs.0 as i32)
    }
}

pub trait BitField<T> {
    fn bf(&self, msb: usize, lsb: usize) -> T;
    fn bit(&self, b: usize) -> bool;
    fn sign_expand(&self, b: usize) -> Self;
    fn zero_expand(&self, b: usize) -> Self;
}

impl BitField<u32> for Word {
    fn bf(&self, msb: usize, lsb: usize) -> u32 {
        (self.0 >> lsb) & ((1 << (msb + 1 - lsb)) - 1)
    }

    fn bit(&self, b: usize) -> bool {
        (self.0 >> b) & 1 != 0
    }

    fn sign_expand(&self, b: usize) -> Word {
        (((self.0 << (32 - b)) as i32 >> (32 - b)) as u32).into_word()
    }

    fn zero_expand(&self, b: usize) -> Word {
        (self << (32 - b)) >> (32 - b)
    }
}

pub trait Instruction<T, W>: BitField<T> {
    fn u_imm(&self) -> W;
    fn j_imm(&self) -> W;
    fn i_imm(&self) -> W;
    fn b_imm(&self) -> W;
    fn s_imm(&self) -> W;
}

impl Instruction<u32, Word> for Word {
    fn u_imm(&self) -> Word {
        (self.bf(31, 12) << 12).into_word()
    }

    fn j_imm(&self) -> Word {
        (self.bf(31, 31) << 20
            | self.bf(19, 12) << 12
            | self.bf(20, 20) << 11
            | self.bf(30, 21) << 1)
            .into_word()
            .sign_expand(21)
    }

    fn i_imm(&self) -> Word {
        self.bf(31, 20).into_word()
    }

    fn b_imm(&self) -> Word {
        (self.bf(31, 31) << 12 | self.bf(7, 7) << 11 | self.bf(30, 25) << 5 | self.bf(11, 8) << 1)
            .into_word()
            .sign_expand(13)
    }

    fn s_imm(&self) -> Word {
        (self.bf(31, 25) << 5 | self.bf(11, 7))
            .into_word()
            .sign_expand(12)
    }
}

#[derive(Debug)]
pub struct Cpu {
    pc: Word,
    regs: [Word; 32],
    pub ram: Vec<Word>,
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "pc: {:08x}", self.pc)?;
        let names = [
            "zero", "  ra", "  sp", "  gp", "  tp", "  t0", "  t1", "  t2", //
            "  s0", "  s1", "  a0", "  a1", "  a2", "  a3", "  a4", "  a5", //
            "  a6", "  a7", "  s2", "  s3", "  s4", "  s5", "  s6", "  s7", //
            "  s8", "  s9", " s10", " s11", "  t3", "  t4", "  t5", "  t6", //
        ];
        for (i, &name) in names.iter().enumerate() {
            write!(f, "{}: {:08x}", name, self.regs[i])?;
            if i % 8 == 7 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl Cpu {
    const RAM_SIZE: usize = 0x10000000;
    const RAM_MASK_IN_WORD: usize = (Self::RAM_SIZE - 1) >> 2;

    pub fn new() -> Cpu {
        Cpu {
            pc: 0x20000000.into_word(),
            regs: [0.into_word(); 32],
            ram: vec![0.into_word(); 0x10],
        }
    }

    fn inst_read(&self, addr: Word) -> Word {
        let a = (addr >> 2).0 as usize;
        if addr & 2.into_word() != 0.into_word() {
            (self.ram[a & Self::RAM_MASK_IN_WORD] >> 16)
                | (self.ram[(a + 1) & Self::RAM_MASK_IN_WORD] & 0xffff.into_word())
        } else {
            self.ram[a & Self::RAM_MASK_IN_WORD]
        }
    }

    fn load_word(&self, addr: Word) -> Word {
        self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD]
    }

    fn load_subword(&self, addr: Word) -> Word {
        self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD] >> ((addr.0 as usize & 3) * 8)
    }

    fn store_word(&mut self, addr: Word, data: Word) {
        self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD] = data;
    }

    fn store_subword(&mut self, addr: Word, data: Word, bits: usize) {
        let old = self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD];
        let mask = ((1.into_word() << bits) - 1.into_word()) << ((addr.0 as usize & 3) * 8);
        self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD] = (old & !mask) | (data & mask);
    }

    fn write_reg(&mut self, rd: u32, val: Word) {
        if rd != 0 {
            self.regs[rd as usize] = val;
        }
    }

    fn read_reg(&mut self, rs: u32) -> Word {
        self.regs[rs as usize]
    }

    pub fn step(&mut self) {
        let inst = self.inst_read(self.pc);
        let opcode = inst.bf(6, 0);
        let mut next_pc: Word;
        let set_if = |b: bool| if b { 1.into_word() } else { 0.into_word() };
        if opcode & 3 == 3 {
            // non-RVC instructions
            next_pc = self.pc + 4.into_word();
            match opcode {
                0x37 => {
                    // lui
                    let rd = inst.bf(11, 7);
                    self.write_reg(rd, inst.u_imm());
                }
                0x17 => {
                    // auipc
                    let rd = inst.bf(11, 7);
                    self.write_reg(rd, self.pc + inst.u_imm());
                }
                0x6f => {
                    // jal
                    let rd = inst.bf(11, 7);
                    self.write_reg(rd, next_pc);
                    next_pc = self.pc + inst.j_imm();
                }
                0x67 => {
                    match inst.bf(14, 12) {
                        0 => {
                            // jalr
                            let rd = inst.bf(11, 7);
                            let rs1 = inst.bf(19, 15);
                            self.write_reg(rd, next_pc);
                            next_pc = (self.read_reg(rs1) & !1.into_word()) + inst.i_imm();
                        }
                        _ => panic!("unknown jalr inst"),
                    }
                }
                0x63 => {
                    // branch
                    let lhs = self.read_reg(inst.bf(19, 15));
                    let rhs = self.read_reg(inst.bf(24, 20));
                    let mut do_branch = |cond| {
                        if cond {
                            next_pc = self.pc + inst.b_imm()
                        }
                    };
                    match inst.bf(14, 12) {
                        0 => do_branch(lhs == rhs),                       // beq
                        1 => do_branch(lhs != rhs),                       // bne
                        4 => do_branch((lhs.0 as i32) < (rhs.0 as i32)),  // blt
                        5 => do_branch((lhs.0 as i32) >= (rhs.0 as i32)), // bge
                        6 => do_branch(lhs < rhs),                        // bltu
                        7 => do_branch(lhs >= rhs),                       // bgeu
                        _ => panic!("unknown branch inst"),
                    }
                }
                0x03 => {
                    // load_
                    let rd = inst.bf(11, 7);
                    let rs1 = inst.bf(19, 15);
                    let addr = self.read_reg(rs1) + inst.i_imm();
                    match inst.bf(14, 12) {
                        0 => self.write_reg(rd, self.load_subword(addr).sign_expand(8)), // lb
                        1 => self.write_reg(rd, self.load_subword(addr).sign_expand(16)), // lh
                        2 => self.write_reg(rd, self.load_word(addr)),                   // lw
                        4 => self.write_reg(rd, self.load_subword(addr).zero_expand(8)), // lbu
                        5 => self.write_reg(rd, self.load_subword(addr).zero_expand(16)), // lhu
                        _ => panic!("unknown load inst"),
                    }
                }
                0x23 => {
                    // store
                    let rs1 = inst.bf(19, 15);
                    let rs2 = inst.bf(24, 20);
                    let data = self.read_reg(rs2);
                    let addr = self.read_reg(rs1) + inst.s_imm();
                    match inst.bf(14, 12) {
                        0 => self.store_subword(addr, data, 8),  // sb
                        1 => self.store_subword(addr, data, 16), // sh
                        2 => self.store_word(addr, data),        // sw
                        _ => panic!("unknown store inst"),
                    }
                }
                0x13 => {
                    // alu imm
                    let rd = inst.bf(11, 7);
                    let rs1 = inst.bf(19, 15);
                    let data1 = self.read_reg(rs1);
                    let data2 = inst.i_imm();
                    let shamt = inst.bf(24, 20) as usize;
                    match (inst.bf(14, 12), inst.bf(31, 25)) {
                        (0, _) => self.write_reg(rd, data1 + data2), // addi
                        (2, _) => self.write_reg(rd, set_if(data1.slt(&data2))), // slti
                        (3, _) => self.write_reg(rd, set_if(data1 < data2)), // sltu
                        (4, _) => self.write_reg(rd, data1 ^ data2), // xori
                        (6, _) => self.write_reg(rd, data1 | data2), // ori
                        (7, _) => self.write_reg(rd, data1 & data2), // andi
                        (1, 0x00) => self.write_reg(rd, data1 << shamt), // slli
                        (5, 0x00) => self.write_reg(rd, data1 >> shamt), // srli
                        (5, 0x20) => self.write_reg(rd, ((data1.0 as i32) >> shamt).into_word()), // srai
                        _ => panic!("unknown alu imm inst"),
                    }
                }
                0x33 => {
                    // alu reg
                    let rd = inst.bf(11, 7);
                    let rs1 = inst.bf(19, 15);
                    let rs2 = inst.bf(24, 20);
                    let data1 = self.read_reg(rs1);
                    let data2 = self.read_reg(rs2);
                    let shamt = (data2.0 as usize) & 31;
                    match (inst.bf(14, 12), inst.bf(31, 25)) {
                        (0, 0x00) => self.write_reg(rd, data1 + data2), // add
                        (0, 0x20) => self.write_reg(rd, data1 - data2), // sub
                        (1, 0x00) => self.write_reg(rd, data1 << shamt), // sll
                        (2, 0x00) => self.write_reg(rd, set_if(data1.slt(&data2))), // slt
                        (3, 0x00) => self.write_reg(rd, set_if(data1 < data2)), // sltu
                        (4, 0x00) => self.write_reg(rd, data1 ^ data2), // xor
                        (5, 0x00) => self.write_reg(rd, data1 >> shamt), // srl
                        (5, 0x20) => self.write_reg(rd, ((data1.0 as i32) >> shamt).into_word()), // sra
                        (6, 0x00) => self.write_reg(rd, data1 | data2), // or
                        (7, 0x00) => self.write_reg(rd, data1 & data2), // and
                        _ => panic!("unknown alu reg inst"),
                    }
                }
                _ => unimplemented!(),
            }
        } else {
            // RVC instructions
            next_pc = self.pc + 2.into_word();
        }
        self.pc = next_pc;
    }
}
