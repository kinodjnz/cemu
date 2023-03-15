use crate::cpu::{BitField, Immediate, Word};
use std::fmt;

#[derive(Debug)]
pub struct Register(u32);

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let names = [
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", //
            "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", //
            "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", //
            "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6", //
        ];
        write!(f, "{}", names[self.0 as usize])
    }
}

#[derive(Debug)]
pub struct Csr(u32);

impl fmt::Display for Csr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let csr_names = [
            (
                0x300,
                [
                    "mstatus",
                    "misa",
                    "medeleg",
                    "mideleg",
                    "mie",
                    "mtvec",
                    "mcounteren",
                ],
            ),
            (
                0x340,
                [
                    "mscratch", "mepc", "mcause", "mtval", "mip", "0x345", "0x346",
                ],
            ),
        ];
        csr_names
            .iter()
            .filter(|(reg, names)| *reg <= self.0 && self.0 < *reg + names.len() as u32)
            .next()
            .map(|(reg, names)| write!(f, "{}", names[(self.0 - reg) as usize]))
            .unwrap_or_else(|| write!(f, "{:#03x}", self.0))
    }
}

#[derive(Debug)]
pub enum Instruction {
    Lui(Register, Word),
    Auipc(Register, Word),
    Jal(Register, Word),
    Jalr(Register, Register, Word),
    Beq(Register, Register, Word),
    Bne(Register, Register, Word),
    Blt(Register, Register, Word),
    Bge(Register, Register, Word),
    Bltu(Register, Register, Word),
    Bgeu(Register, Register, Word),
    Lb(Register, Register, Word),
    Lh(Register, Register, Word),
    Lw(Register, Register, Word),
    Lbu(Register, Register, Word),
    Lhu(Register, Register, Word),
    Sb(Register, Register, Word),
    Sh(Register, Register, Word),
    Sw(Register, Register, Word),
    Addi(Register, Register, Word),
    Slti(Register, Register, Word),
    Sltiu(Register, Register, Word),
    Xori(Register, Register, Word),
    Ori(Register, Register, Word),
    Andi(Register, Register, Word),
    Slli(Register, Register, usize),
    Srli(Register, Register, usize),
    Srai(Register, Register, usize),
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Sll(Register, Register, Register),
    Slt(Register, Register, Register),
    Sltu(Register, Register, Register),
    Xor(Register, Register, Register),
    Srl(Register, Register, Register),
    Sra(Register, Register, Register),
    Or(Register, Register, Register),
    And(Register, Register, Register),
    Csrrw(Register, Csr, Register),
    Csrrs(Register, Csr, Register),
    Csrrc(Register, Csr, Register),
    Csrrwi(Register, Csr, Word),
    Csrrsi(Register, Csr, Word),
    Csrrci(Register, Csr, Word),
    Mul(Register, Register, Register),
    Mulh(Register, Register, Register),
    Mulhsu(Register, Register, Register),
    Mulhu(Register, Register, Register),
    Div(Register, Register, Register),
    Divu(Register, Register, Register),
    Rem(Register, Register, Register),
    Remu(Register, Register, Register),
    CAddi4spn(Register, Word),
    CLw(Register, Register, Word),
    CSw(Register, Register, Word),
    CNop(),
    CAddi(Register, Word),
    CJal(Word),
    CLi(Register, Word),
    CAddi16sp(Word),
    CLui(Register, Word),
    CSrli(Register, usize),
    CSrai(Register, usize),
    CAndi(Register, Word),
    CSub(Register, Register),
    CXor(Register, Register),
    COr(Register, Register),
    CAnd(Register, Register),
    CJ(Word),
    CBeqz(Register, Word),
    CBnez(Register, Word),
    CSlli(Register, usize),
    CLwsp(Register, Word),
    CJr(Register),
    CMv(Register, Register),
    CEbreak(),
    CJalr(Register),
    CAdd(Register, Register),
    CSwsp(Register, Word),
    Unknown(Word),
}

use Instruction::*;

pub fn disasm(inst: Word) -> Instruction {
    let opcode = inst.bf(6, 0);
    if opcode & 3 == 3 {
        // non-RVC instructions
        let rd = Register(inst.bf(11, 7));
        let rs1 = Register(inst.bf(19, 15));
        let rs2 = Register(inst.bf(24, 20));
        let csr = Csr(inst.bf(31, 20));
        match opcode {
            0x37 => Instruction::Lui(rd, inst.u_imm()),
            0x17 => Instruction::Auipc(rd, inst.u_imm()),
            0x6f => Instruction::Jal(rd, inst.j_imm()),
            0x67 => match inst.bf(14, 12) {
                0 => Instruction::Jalr(rd, rs1, inst.i_imm()),
                _ => Instruction::Unknown(inst),
            },
            0x63 => match inst.bf(14, 12) {
                0 => Instruction::Beq(rs1, rs2, inst.b_imm()),
                1 => Instruction::Bne(rs1, rs2, inst.b_imm()),
                4 => Instruction::Blt(rs1, rs2, inst.b_imm()),
                5 => Instruction::Bge(rs1, rs2, inst.b_imm()),
                6 => Instruction::Bltu(rs1, rs2, inst.b_imm()),
                7 => Instruction::Bgeu(rs1, rs2, inst.b_imm()),
                _ => Instruction::Unknown(inst),
            },
            0x03 => match inst.bf(14, 12) {
                0 => Instruction::Lb(rd, rs1, inst.i_imm()),
                1 => Instruction::Lh(rd, rs1, inst.i_imm()),
                2 => Instruction::Lw(rd, rs1, inst.i_imm()),
                4 => Instruction::Lbu(rd, rs1, inst.i_imm()),
                5 => Instruction::Lhu(rd, rs1, inst.i_imm()),
                _ => Instruction::Unknown(inst),
            },
            0x23 => match inst.bf(14, 12) {
                0 => Instruction::Sb(rs2, rs1, inst.s_imm()),
                1 => Instruction::Sh(rs2, rs1, inst.s_imm()),
                2 => Instruction::Sw(rs2, rs1, inst.s_imm()),
                _ => Instruction::Unknown(inst),
            },
            0x13 => match (inst.bf(14, 12), inst.bf(31, 25)) {
                (0, _) => Instruction::Addi(rd, rs1, inst.i_imm()),
                (2, _) => Instruction::Slti(rd, rs1, inst.i_imm()),
                (3, _) => Instruction::Sltiu(rd, rs1, inst.i_imm()),
                (4, _) => Instruction::Xori(rd, rs1, inst.i_imm()),
                (6, _) => Instruction::Ori(rd, rs1, inst.i_imm()),
                (7, _) => Instruction::Andi(rd, rs1, inst.i_imm()),
                (1, 0x00) => Instruction::Slli(rd, rs1, inst.bf(24, 20) as usize),
                (5, 0x00) => Instruction::Srli(rd, rs1, inst.bf(24, 20) as usize),
                (5, 0x20) => Instruction::Srai(rd, rs1, inst.bf(24, 20) as usize),
                _ => Instruction::Unknown(inst),
            },
            0x33 => match (inst.bf(14, 12), inst.bf(31, 25)) {
                (0, 0x00) => Instruction::Add(rd, rs1, rs2),
                (0, 0x20) => Instruction::Sub(rd, rs1, rs2),
                (1, 0x00) => Instruction::Sll(rd, rs1, rs2),
                (2, 0x00) => Instruction::Slt(rd, rs1, rs2),
                (3, 0x00) => Instruction::Sltu(rd, rs1, rs2),
                (4, 0x00) => Instruction::Xor(rd, rs1, rs2),
                (5, 0x00) => Instruction::Srl(rd, rs1, rs2),
                (5, 0x20) => Instruction::Sra(rd, rs1, rs2),
                (6, 0x00) => Instruction::Or(rd, rs1, rs2),
                (7, 0x00) => Instruction::And(rd, rs1, rs2),
                (0, 0x01) => Instruction::Mul(rd, rs1, rs2),
                (1, 0x01) => Instruction::Mulh(rd, rs1, rs2),
                (2, 0x01) => Instruction::Mulhsu(rd, rs1, rs2),
                (3, 0x01) => Instruction::Mulhu(rd, rs1, rs2),
                (4, 0x01) => Instruction::Div(rd, rs1, rs2),
                (5, 0x01) => Instruction::Divu(rd, rs1, rs2),
                (6, 0x01) => Instruction::Rem(rd, rs1, rs2),
                (7, 0x01) => Instruction::Remu(rd, rs1, rs2),
                _ => Instruction::Unknown(inst),
            },
            0x73 => match inst.bf(14, 12) {
                1 => Instruction::Csrrw(rd, csr, rs1),
                2 => Instruction::Csrrs(rd, csr, rs1),
                3 => Instruction::Csrrc(rd, csr, rs1),
                5 => Instruction::Csrrwi(rd, csr, inst.csr_uimm()),
                6 => Instruction::Csrrsi(rd, csr, inst.csr_uimm()),
                7 => Instruction::Csrrci(rd, csr, inst.csr_uimm()),
                _ => Instruction::Unknown(inst),
            },
            _ => Instruction::Unknown(inst),
        }
    } else {
        let rd = Register(inst.bf(11, 7));
        let rs = Register(inst.bf(6, 2));
        let r1c = Register(inst.bf(9, 7) + 8);
        let r2c = Register(inst.bf(4, 2) + 8);
        match (opcode & 3, inst.bf(15, 13)) {
            (0, 0) => Instruction::CAddi4spn(r2c, inst.ciw_uimm()),
            (0, 2) => Instruction::CLw(r2c, r1c, inst.clw_uimm()),
            (0, 6) => Instruction::CSw(r2c, r1c, inst.csw_uimm()),
            (1, 0) => match rd {
                Register(0) if inst.ci_imm().0 == 0 => Instruction::CNop(),
                _ => Instruction::CAddi(rd, inst.ci_imm()),
            },
            (1, 1) => Instruction::CJal(inst.cj_imm()),
            (1, 2) => Instruction::CLi(rd, inst.ci_imm()),
            (1, 3) => match rd {
                Register(0) => Instruction::Unknown(inst),
                Register(2) => Instruction::CAddi16sp(inst.ci16_imm()),
                _ => Instruction::CLui(rd, inst.ciu_imm()),
            },
            (1, 4) => match (inst.bf(11, 10), inst.bf(6, 5), inst.bf(12, 12)) {
                (0, _, _) => Instruction::CSrli(r1c, inst.ci_shamt()),
                (1, _, _) => Instruction::CSrai(r1c, inst.ci_shamt()),
                (2, _, _) => Instruction::CAndi(r1c, inst.ci_imm()),
                (3, 0, 0) => Instruction::CSub(r1c, r2c),
                (3, 1, 0) => Instruction::CXor(r1c, r2c),
                (3, 2, 0) => Instruction::COr(r1c, r2c),
                (3, 3, 0) => Instruction::CAnd(r1c, r2c),
                _ => Instruction::Unknown(inst),
            },
            (1, 5) => Instruction::CJ(inst.cj_imm()),
            (1, 6) => Instruction::CBeqz(r1c, inst.cb_imm()),
            (1, 7) => Instruction::CBnez(r1c, inst.cb_imm()),
            (2, 0) => Instruction::CSlli(rd, inst.ci_shamt()),
            (2, 2) => Instruction::CLwsp(rd, inst.clwsp_uimm()),
            (2, 4) => match (inst.bf(12, 12), rd.0, rs.0) {
                (0, rs, 0) if rs != 0 => Instruction::CJr(Register(rs)),
                (0, rd, rs) if rd != 0 => Instruction::CMv(Register(rd), Register(rs)),
                (1, 0, 0) => Instruction::CEbreak(),
                (1, rs, 0) => Instruction::CJalr(Register(rs)),
                (1, rd, rs) => Instruction::CAdd(Register(rd), Register(rs)),
                _ => Instruction::Unknown(inst),
            },
            (2, 6) => Instruction::CSwsp(rs, inst.cswsp_uimm()),
            _ => Instruction::Unknown(inst),
        }
    }
}

pub struct LocatedInstruction<'a>(&'a Instruction, Word);

impl fmt::Display for LocatedInstruction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Lui(rd, imm) => write!(f, "lui {}, {:#08x}", rd, imm),
            Auipc(rd, imm) => write!(f, "auipc {}, {:#08x}", rd, self.1 + imm),
            Jal(rd, imm) => write!(f, "jal {}, {:#08x}", rd, self.1 + imm),
            Jalr(rd, rs1, imm) => write!(f, "jalr {}, {}({})", rd, imm.0 as i32, rs1),
            Beq(rs1, rs2, imm) => write!(f, "beq {}, {}, {:#08x}", rs1, rs2, self.1 + imm),
            Bne(rs1, rs2, imm) => write!(f, "bne {}, {}, {:#08x}", rs1, rs2, self.1 + imm),
            Blt(rs1, rs2, imm) => write!(f, "blt {}, {}, {:#08x}", rs1, rs2, self.1 + imm),
            Bge(rs1, rs2, imm) => write!(f, "bge {}, {}, {:#08x}", rs1, rs2, self.1 + imm),
            Bltu(rs1, rs2, imm) => write!(f, "bltu {}, {}, {:#08x}", rs1, rs2, self.1 + imm),
            Bgeu(rs1, rs2, imm) => write!(f, "bgeu {}, {}, {:#08x}", rs1, rs2, self.1 + imm),
            Lb(rd, rs1, imm) => write!(f, "lb {}, {}({})", rd, imm.0 as i32, rs1),
            Lh(rd, rs1, imm) => write!(f, "lh {}, {}({})", rd, imm.0 as i32, rs1),
            Lw(rd, rs1, imm) => write!(f, "lw {}, {}({})", rd, imm.0 as i32, rs1),
            Lbu(rd, rs1, imm) => write!(f, "lbu {}, {}({})", rd, imm.0 as i32, rs1),
            Lhu(rd, rs1, imm) => write!(f, "lhu {}, {}({})", rd, imm.0 as i32, rs1),
            Sb(rs2, rs1, imm) => write!(f, "sb {}, {}({})", rs2, imm.0 as i32, rs1),
            Sh(rs2, rs1, imm) => write!(f, "sh {}, {}({})", rs2, imm.0 as i32, rs1),
            Sw(rs2, rs1, imm) => write!(f, "sw {}, {}({})", rs2, imm.0 as i32, rs1),
            Addi(rd, rs1, imm) => write!(f, "addi {}, {}, {}", rd, rs1, imm.0 as i32),
            Slti(rd, rs1, imm) => write!(f, "slti {}, {}, {}", rd, rs1, imm.0 as i32),
            Sltiu(rd, rs1, imm) => write!(f, "sltiu {}, {}, {}", rd, rs1, imm),
            Xori(rd, rs1, imm) => write!(f, "xori {}, {}, {}", rd, rs1, imm.0 as i32),
            Ori(rd, rs1, imm) => write!(f, "ori {}, {}, {}", rd, rs1, imm.0 as i32),
            Andi(rd, rs1, imm) => write!(f, "andi {}, {}, {}", rd, rs1, imm.0 as i32),
            Slli(rd, rs1, shamt) => write!(f, "slli {}, {}, {}", rd, rs1, shamt),
            Srli(rd, rs1, shamt) => write!(f, "srli {}, {}, {}", rd, rs1, shamt),
            Srai(rd, rs1, shamt) => write!(f, "srai {}, {}, {}", rd, rs1, shamt),
            Add(rd, rs1, rs2) => write!(f, "add {}, {}, {}", rd, rs1, rs2),
            Sub(rd, rs1, rs2) => write!(f, "sub {}, {}, {}", rd, rs1, rs2),
            Sll(rd, rs1, rs2) => write!(f, "sll {}, {}, {}", rd, rs1, rs2),
            Slt(rd, rs1, rs2) => write!(f, "slt {}, {}, {}", rd, rs1, rs2),
            Sltu(rd, rs1, rs2) => write!(f, "sltu {}, {}, {}", rd, rs1, rs2),
            Xor(rd, rs1, rs2) => write!(f, "xor {}, {}, {}", rd, rs1, rs2),
            Srl(rd, rs1, rs2) => write!(f, "srl {}, {}, {}", rd, rs1, rs2),
            Sra(rd, rs1, rs2) => write!(f, "sra {}, {}, {}", rd, rs1, rs2),
            Or(rd, rs1, rs2) => write!(f, "or {}, {}, {}", rd, rs1, rs2),
            And(rd, rs1, rs2) => write!(f, "and {}, {}, {}", rd, rs1, rs2),
            Csrrw(rd, csr, rs1) => write!(f, "csrrw {}, {}, {}", rd, csr, rs1),
            Csrrs(rd, csr, rs1) => write!(f, "csrrs {}, {}, {}", rd, csr, rs1),
            Csrrc(rd, csr, rs1) => write!(f, "csrrc {}, {}, {}", rd, csr, rs1),
            Csrrwi(rd, csr, imm) => write!(f, "csrrwi {}, {}, {}", rd, csr, imm),
            Csrrsi(rd, csr, imm) => write!(f, "csrrsi {}, {}, {}", rd, csr, imm),
            Csrrci(rd, csr, imm) => write!(f, "csrrci {}, {}, {}", rd, csr, imm),
            Mul(rd, rs1, rs2) => write!(f, "mul {}, {}, {}", rd, rs1, rs2),
            Mulh(rd, rs1, rs2) => write!(f, "mulh {}, {}, {}", rd, rs1, rs2),
            Mulhsu(rd, rs1, rs2) => write!(f, "mulhsu {}, {}, {}", rd, rs1, rs2),
            Mulhu(rd, rs1, rs2) => write!(f, "mulhu {}, {}, {}", rd, rs1, rs2),
            Div(rd, rs1, rs2) => write!(f, "div {}, {}, {}", rd, rs1, rs2),
            Divu(rd, rs1, rs2) => write!(f, "divu {}, {}, {}", rd, rs1, rs2),
            Rem(rd, rs1, rs2) => write!(f, "rem {}, {}, {}", rd, rs1, rs2),
            Remu(rd, rs1, rs2) => write!(f, "remu {}, {}, {}", rd, rs1, rs2),
            CAddi4spn(rd, imm) => write!(f, "addi {}, sp, {}", rd, imm),
            CLw(rd, rs1, imm) => write!(f, "lw {}, {}({})", rd, imm, rs1),
            CSw(rs2, rs1, imm) => write!(f, "sw {}, {}({})", rs2, imm, rs1),
            CNop() => write!(f, "nop"),
            CAddi(rd, imm) => write!(f, "addi {}, {}, {}", rd, rd, imm.0 as i32),
            CJal(imm) => write!(f, "jal ra, {:#08x}", self.1 + imm),
            CLi(rd, imm) => write!(f, "li {}, {}", rd, imm.0 as i32),
            CAddi16sp(imm) => write!(f, "addi sp, sp, {}", imm.0 as i32),
            CLui(rd, imm) => write!(f, "lui {}, {:#08x}", rd, imm),
            CSrli(rd, shamt) => write!(f, "srli {}, {}, {}", rd, rd, shamt),
            CSrai(rd, shamt) => write!(f, "srai {}, {}, {}", rd, rd, shamt),
            CAndi(rd, imm) => write!(f, "andi {}, {}, {}", rd, rd, imm.0 as i32),
            CSub(rd, rs) => write!(f, "sub {}, {}, {}", rd, rd, rs),
            CXor(rd, rs) => write!(f, "xor {}, {}, {}", rd, rd, rs),
            COr(rd, rs) => write!(f, "or {}, {}, {}", rd, rd, rs),
            CAnd(rd, rs) => write!(f, "and {}, {}, {}", rd, rd, rs),
            CJ(imm) => write!(f, "j {:#08x}", self.1 + imm),
            CBeqz(rs, imm) => write!(f, "beqz {}, {:#08x}", rs, self.1 + imm),
            CBnez(rs, imm) => write!(f, "bnez {}, {:#08x}", rs, self.1 + imm),
            CSlli(rd, shamt) => write!(f, "slli {}, {}, {}", rd, rd, shamt),
            CLwsp(rd, imm) => write!(f, "lw {}, {}(sp)", rd, imm.0 as i32),
            CJr(rd) => write!(f, "jalr zero, 0({})", rd),
            CMv(rd, rs) => write!(f, "mv {}, {}", rd, rs),
            CEbreak() => write!(f, "ebreak"),
            CJalr(rs) => write!(f, "jalr ra, 0({})", rs),
            CAdd(rd, rs) => write!(f, "add {}, {}, {}", rd, rd, rs),
            CSwsp(rs, imm) => write!(f, "sw {}, {}(sp)", rs, imm),
            _ => write!(f, "{:?}", self.0),
        }
    }
}

impl Instruction {
    pub fn with_pc(&self, pc: Word) -> LocatedInstruction {
        LocatedInstruction(self, pc)
    }
}
