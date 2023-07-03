use crate::cpu::{BitField, Immediate, Word};
use std::fmt;
use superslice::*;

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
            (0x300, "mstatus"),
            (0x301, "misa"),
            (0x302, "medeleg"),
            (0x303, "mideleg"),
            (0x304, "mie"),
            (0x305, "mtvec"),
            (0x306, "mcounteren"),
            (0x340, "mscratch"),
            (0x341, "mepc"),
            (0x342, "mcause"),
            (0x343, "mtval"),
            (0x344, "mip"),
        ];
        let i = csr_names.equal_range_by_key(&self.0, |(reg, _)| *reg);
        if i.is_empty() {
            write!(f, "{:#03x}", self.0)
        } else {
            write!(f, "{}", csr_names[i.start].1)
        }
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
    Ecall(),
    Mret(),
    Mul(Register, Register, Register),
    Mulh(Register, Register, Register),
    Mulhsu(Register, Register, Register),
    Mulhu(Register, Register, Register),
    Div(Register, Register, Register),
    Divu(Register, Register, Register),
    Rem(Register, Register, Register),
    Remu(Register, Register, Register),
    CMov(Register, Register, Register, Register),
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
    CLb(Register, Register, Word),
    CLbu(Register, Register, Word),
    CLh(Register, Register, Word),
    CLhu(Register, Register, Word),
    CSb(Register, Register, Word),
    CSh(Register, Register, Word),
    CSb0(Register, Word),
    CSh0(Register, Word),
    CSw0(Register, Word),
    CAuipc(Register, Word),
    CMul(Register, Register),
    CZextB(Register),
    CSextB(Register),
    CZextH(Register),
    CSextH(Register),
    CNot(Register),
    CNeg(Register),
    CBeq(Register, Register, Word),
    CBne(Register, Register, Word),
    CAddi2w(Register, Register, Word),
    CAdd2(Register, Register, Register),
    CSeqz(Register, Register),
    CSnez(Register, Register),
    CAddi2b(Register, Register, Word),
    CSlt(Register, Register, Register),
    CSltu(Register, Register, Register),
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
        let rs3 = Register(inst.bf(31, 27));
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
            0x33 => match (inst.bf(14, 12), inst.bf(26, 26), inst.bf(31, 27), inst.bf(25, 25)) {
                (0, 0, 0x00, 0) => Instruction::Add(rd, rs1, rs2),
                (0, 0, 0x08, 0) => Instruction::Sub(rd, rs1, rs2),
                (1, 0, 0x00, 0) => Instruction::Sll(rd, rs1, rs2),
                (2, 0, 0x00, 0) => Instruction::Slt(rd, rs1, rs2),
                (3, 0, 0x00, 0) => Instruction::Sltu(rd, rs1, rs2),
                (4, 0, 0x00, 0) => Instruction::Xor(rd, rs1, rs2),
                (5, 0, 0x00, 0) => Instruction::Srl(rd, rs1, rs2),
                (5, 0, 0x08, 0) => Instruction::Sra(rd, rs1, rs2),
                (6, 0, 0x00, 0) => Instruction::Or(rd, rs1, rs2),
                (7, 0, 0x00, 0) => Instruction::And(rd, rs1, rs2),
                (0, 0, 0x00, 1) => Instruction::Mul(rd, rs1, rs2),
                (1, 0, 0x00, 1) => Instruction::Mulh(rd, rs1, rs2),
                (2, 0, 0x00, 1) => Instruction::Mulhsu(rd, rs1, rs2),
                (3, 0, 0x00, 1) => Instruction::Mulhu(rd, rs1, rs2),
                (4, 0, 0x00, 1) => Instruction::Div(rd, rs1, rs2),
                (5, 0, 0x00, 1) => Instruction::Divu(rd, rs1, rs2),
                (6, 0, 0x00, 1) => Instruction::Rem(rd, rs1, rs2),
                (7, 0, 0x00, 1) => Instruction::Remu(rd, rs1, rs2),
                (5, 1, _, 1) => Instruction::CMov(rd, rs2, rs1, rs3),
                _ => Instruction::Unknown(inst),
            },
            0x73 => match inst.0 {
                0x00000073 => Instruction::Ecall(),
                0x30200073 => Instruction::Mret(),
                _ => match inst.bf(14, 12) {
                    1 => Instruction::Csrrw(rd, csr, rs1),
                    2 => Instruction::Csrrs(rd, csr, rs1),
                    3 => Instruction::Csrrc(rd, csr, rs1),
                    5 => Instruction::Csrrwi(rd, csr, inst.csr_uimm()),
                    6 => Instruction::Csrrsi(rd, csr, inst.csr_uimm()),
                    7 => Instruction::Csrrci(rd, csr, inst.csr_uimm()),
                    _ => Instruction::Unknown(inst),
                },
            },
            _ => Instruction::Unknown(inst),
        }
    } else {
        let rd = Register(inst.bf(11, 7));
        let rs = Register(inst.bf(6, 2));
        let r1c = Register(inst.bf(9, 7) + 8);
        let r2c = Register(inst.bf(4, 2) + 8);
        let r3c = Register(inst.bf(12, 10) + 8);
        match (opcode & 3, inst.bf(15, 13)) {
            (0, 0) => Instruction::CAddi4spn(r2c, inst.ciw_uimm()),
            (0, 1) => Instruction::CLb(r2c, r1c, inst.cclsb_uimm()),
            (0, 2) => Instruction::CLw(r2c, r1c, inst.clw_uimm()),
            (0, 3) => Instruction::CLbu(r2c, r1c, inst.cclsb_uimm()),
            (0, 4) => Instruction::CBeq(r1c, r2c, inst.ccb_imm()),
            (0, 5) => Instruction::CBne(r1c, r2c, inst.ccb_imm()),
            (0, 6) => Instruction::CSw(r2c, r1c, inst.csw_uimm()),
            (0, 7) => Instruction::CAuipc(r2c, inst.ccaui_uimm()),
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
                (3, 2, 1) => Instruction::CMul(r1c, r2c),
                (3, 3, 0) => Instruction::CAnd(r1c, r2c),
                (3, 3, 1) => match inst.bf(4, 2) {
                    0 => Instruction::CZextB(r1c),
                    1 => Instruction::CSextB(r1c),
                    2 => Instruction::CZextH(r1c),
                    3 => Instruction::CSextH(r1c),
                    5 => Instruction::CNot(r1c),
                    6 => Instruction::CNeg(r1c),
                    _ => Instruction::Unknown(inst),
                }
                _ => Instruction::Unknown(inst),
            },
            (1, 5) => Instruction::CJ(inst.cj_imm()),
            (1, 6) => Instruction::CBeqz(r1c, inst.cb_imm()),
            (1, 7) => Instruction::CBnez(r1c, inst.cb_imm()),
            (2, 0) => Instruction::CSlli(rd, inst.ci_shamt()),
            (2, 1) => match inst.bf(12, 11) {
                0 => Instruction::CLh(r2c, r1c, inst.cclsh_uimm()),
                1 => Instruction::CLhu(r2c, r1c, inst.cclsh_uimm()),
                2 => Instruction::CSh(r2c, r1c, inst.cclsh_uimm()),
                3 => match (inst.bf(2, 2), inst.bf(3, 3)) {
                    (0, _) => Instruction::CSw0(r1c, inst.ccsw0_uimm()),
                    (1, 0) => Instruction::CSb0(r1c, inst.ccsb0_uimm()),
                    (1, 1) => Instruction::CSh0(r1c, inst.ccsh0_uimm()),
                    _ => Instruction::Unknown(inst),
                }
                _ => Instruction::Unknown(inst),
            }
            (2, 2) => Instruction::CLwsp(rd, inst.clwsp_uimm()),
            (2, 3) => Instruction::CSb(r2c, r1c, inst.cclsb_uimm()),
            (2, 4) => match (inst.bf(12, 12), rd.0, rs.0) {
                (0, rs, 0) if rs != 0 => Instruction::CJr(Register(rs)),
                (0, rd, rs) if rd != 0 => Instruction::CMv(Register(rd), Register(rs)),
                (1, 0, 0) => Instruction::CEbreak(),
                (1, rs, 0) => Instruction::CJalr(Register(rs)),
                (1, rd, rs) => Instruction::CAdd(Register(rd), Register(rs)),
                _ => Instruction::Unknown(inst),
            },
            (2, 5) => Instruction::CAddi2w(r2c, r1c, inst.cca2w_imm()),
            (2, 6) => Instruction::CSwsp(rs, inst.cswsp_uimm()),
            (2, 7) => match inst.bf(6, 5) {
                0 => Instruction::CAdd2(r2c, r1c, r3c),
                1 => match inst.bf(12, 10) {
                    0 => Instruction::CSeqz(r2c, r1c),
                    4 => Instruction::CSnez(r2c, r1c),
                    _ => Instruction::CAddi2b(r2c, r1c, inst.cca2b_imm()),
                }
                2 => Instruction::CSlt(r2c, r1c, r3c),
                3 => Instruction::CSltu(r2c, r1c, r3c),
                _ => Instruction::Unknown(inst),
            }
            _ => Instruction::Unknown(inst),
        }
    }
}

pub struct LocatedInstruction<'a>(&'a Instruction, Word);

impl fmt::Display for LocatedInstruction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Lui(rd, imm) => write!(f, "lui {rd}, {imm:#010x}"),
            Auipc(rd, imm) => write!(f, "auipc {rd}, {:#010x}", self.1 + imm),
            Jal(rd, imm) => write!(f, "jal {rd}, {:#010x}", self.1 + imm),
            Jalr(rd, rs1, imm) => write!(f, "jalr {rd}, {}({rs1})", imm.0 as i32),
            Beq(rs1, rs2, imm) => write!(f, "beq {rs1}, {rs2}, {:#010x}", self.1 + imm),
            Bne(rs1, rs2, imm) => write!(f, "bne {rs1}, {rs2}, {:#010x}", self.1 + imm),
            Blt(rs1, rs2, imm) => write!(f, "blt {rs1}, {rs2}, {:#010x}", self.1 + imm),
            Bge(rs1, rs2, imm) => write!(f, "bge {rs1}, {rs2}, {:#010x}", self.1 + imm),
            Bltu(rs1, rs2, imm) => write!(f, "bltu {rs1}, {rs2}, {:#010x}", self.1 + imm),
            Bgeu(rs1, rs2, imm) => write!(f, "bgeu {rs1}, {rs2}, {:#010x}", self.1 + imm),
            Lb(rd, rs1, imm) => write!(f, "lb {rd}, {}({rs1})", imm.0 as i32),
            Lh(rd, rs1, imm) => write!(f, "lh {rd}, {}({rs1})", imm.0 as i32),
            Lw(rd, rs1, imm) => write!(f, "lw {rd}, {}({rs1})", imm.0 as i32),
            Lbu(rd, rs1, imm) => write!(f, "lbu {rd}, {}({rs1})", imm.0 as i32),
            Lhu(rd, rs1, imm) => write!(f, "lhu {rd}, {}({rs1})", imm.0 as i32),
            Sb(rs2, rs1, imm) => write!(f, "sb {rs2}, {}({rs1})", imm.0 as i32),
            Sh(rs2, rs1, imm) => write!(f, "sh {rs2}, {}({rs1})", imm.0 as i32),
            Sw(rs2, rs1, imm) => write!(f, "sw {rs2}, {}({rs1})", imm.0 as i32),
            Addi(rd, rs1, imm) => write!(f, "addi {rd}, {rs1}, {}", imm.0 as i32),
            Slti(rd, rs1, imm) => write!(f, "slti {rd}, {rs1}, {}", imm.0 as i32),
            Sltiu(rd, rs1, imm) => write!(f, "sltiu {rd}, {rs1}, {imm}"),
            Xori(rd, rs1, imm) => write!(f, "xori {rd}, {rs1}, {}", imm.0 as i32),
            Ori(rd, rs1, imm) => write!(f, "ori {rd}, {rs1}, {}", imm.0 as i32),
            Andi(rd, rs1, imm) => write!(f, "andi {rd}, {rs1}, {}", imm.0 as i32),
            Slli(rd, rs1, shamt) => write!(f, "slli {rd}, {rs1}, {shamt}"),
            Srli(rd, rs1, shamt) => write!(f, "srli {rd}, {rs1}, {shamt}"),
            Srai(rd, rs1, shamt) => write!(f, "srai {rd}, {rs1}, {shamt}"),
            Add(rd, rs1, rs2) => write!(f, "add {rd}, {rs1}, {rs2}"),
            Sub(rd, rs1, rs2) => write!(f, "sub {rd}, {rs1}, {rs2}"),
            Sll(rd, rs1, rs2) => write!(f, "sll {rd}, {rs1}, {rs2}"),
            Slt(rd, rs1, rs2) => write!(f, "slt {rd}, {rs1}, {rs2}"),
            Sltu(rd, rs1, rs2) => write!(f, "sltu {rd}, {rs1}, {rs2}"),
            Xor(rd, rs1, rs2) => write!(f, "xor {rd}, {rs1}, {rs2}"),
            Srl(rd, rs1, rs2) => write!(f, "srl {rd}, {rs1}, {rs2}"),
            Sra(rd, rs1, rs2) => write!(f, "sra {rd}, {rs1}, {rs2}"),
            Or(rd, rs1, rs2) => write!(f, "or {rd}, {rs1}, {rs2}"),
            And(rd, rs1, rs2) => write!(f, "and {rd}, {rs1}, {rs2}"),
            Csrrw(rd, csr, rs1) => write!(f, "csrrw {rd}, {csr}, {rs1}"),
            Csrrs(rd, csr, rs1) => write!(f, "csrrs {rd}, {csr}, {rs1}"),
            Csrrc(rd, csr, rs1) => write!(f, "csrrc {rd}, {csr}, {rs1}"),
            Csrrwi(rd, csr, imm) => write!(f, "csrrwi {rd}, {csr}, {imm}"),
            Csrrsi(rd, csr, imm) => write!(f, "csrrsi {rd}, {csr}, {imm}"),
            Csrrci(rd, csr, imm) => write!(f, "csrrci {rd}, {csr}, {imm}"),
            Ecall() => write!(f, "ecall"),
            Mret() => write!(f, "mret"),
            Mul(rd, rs1, rs2) => write!(f, "mul {rd}, {rs1}, {rs2}"),
            Mulh(rd, rs1, rs2) => write!(f, "mulh {rd}, {rs1}, {rs2}"),
            Mulhsu(rd, rs1, rs2) => write!(f, "mulhsu {rd}, {rs1}, {rs2}"),
            Mulhu(rd, rs1, rs2) => write!(f, "mulhu {rd}, {rs1}, {rs2}"),
            Div(rd, rs1, rs2) => write!(f, "div {rd}, {rs1}, {rs2}"),
            Divu(rd, rs1, rs2) => write!(f, "divu {rd}, {rs1}, {rs2}"),
            Rem(rd, rs1, rs2) => write!(f, "rem {rd}, {rs1}, {rs2}"),
            Remu(rd, rs1, rs2) => write!(f, "remu {rd}, {rs1}, {rs2}"),
            CMov(rd, rs2, rs1, rs3) => write!(f, "cmov {rd}, {rs2}, {rs1}, {rs3}"),
            CAddi4spn(rd, imm) => write!(f, "addi {rd}, sp, {imm}"),
            CLw(rd, rs1, imm) => write!(f, "lw {rd}, {imm}({rs1})"),
            CSw(rs2, rs1, imm) => write!(f, "sw {rs2}, {imm}({rs1})"),
            CNop() => write!(f, "nop"),
            CAddi(rd, imm) => write!(f, "addi {rd}, {rd}, {}", imm.0 as i32),
            CJal(imm) => write!(f, "jal ra, {:#010x}", self.1 + imm),
            CLi(rd, imm) => write!(f, "li {rd}, {}", imm.0 as i32),
            CAddi16sp(imm) => write!(f, "addi sp, sp, {}", imm.0 as i32),
            CLui(rd, imm) => write!(f, "lui {rd}, {imm:#010x}"),
            CSrli(rd, shamt) => write!(f, "srli {rd}, {rd}, {shamt}"),
            CSrai(rd, shamt) => write!(f, "srai {rd}, {rd}, {shamt}"),
            CAndi(rd, imm) => write!(f, "andi {rd}, {rd}, {}", imm.0 as i32),
            CSub(rd, rs) => write!(f, "sub {rd}, {rd}, {rs}"),
            CXor(rd, rs) => write!(f, "xor {rd}, {rd}, {rs}"),
            COr(rd, rs) => write!(f, "or {rd}, {rd}, {rs}"),
            CAnd(rd, rs) => write!(f, "and {rd}, {rd}, {rs}"),
            CJ(imm) => write!(f, "j {:#010x}", self.1 + imm),
            CBeqz(rs, imm) => write!(f, "beqz {rs}, {:#010x}", self.1 + imm),
            CBnez(rs, imm) => write!(f, "bnez {rs}, {:#010x}", self.1 + imm),
            CSlli(rd, shamt) => write!(f, "slli {rd}, {rd}, {shamt}"),
            CLwsp(rd, imm) => write!(f, "lw {rd}, {}(sp)", imm.0 as i32),
            CJr(rd) => write!(f, "jalr zero, 0({rd})"),
            CMv(rd, rs) => write!(f, "mv {rd}, {rs}"),
            CEbreak() => write!(f, "ebreak"),
            CJalr(rs) => write!(f, "jalr ra, 0({rs})"),
            CAdd(rd, rs) => write!(f, "add {rd}, {rd}, {rs}"),
            CSwsp(rs, imm) => write!(f, "sw {rs}, {imm}(sp)"),
            CLb(rd, rs1, imm) => write!(f, "lb {rd}, {imm}({rs1})"),
            CLbu(rd, rs1, imm) => write!(f, "lbu {rd}, {imm}({rs1})"),
            CLh(rd, rs1, imm) => write!(f, "lh {rd}, {imm}({rs1})"),
            CLhu(rd, rs1, imm) => write!(f, "lhu {rd}, {imm}({rs1})"),
            CSb(rs2, rs1, imm) => write!(f, "sb {rs2}, {imm}({rs1})"),
            CSh(rs2, rs1, imm) => write!(f, "sh {rs2}, {imm}({rs1})"),
            CSb0(rs1, imm) => write!(f, "sb zero, {imm}({rs1})"),
            CSh0(rs1, imm) => write!(f, "sh zero, {imm}({rs1})"),
            CSw0(rs1, imm) => write!(f, "sw zero, {imm}({rs1})"),
            CAuipc(rd, imm) => write!(f, "auipc {rd}, {:#010x}", self.1 + imm),
            CMul(rd, rs) => write!(f, "mul {rd}, {rd}, {rs}"),
            CZextB(rd) => write!(f, "zext.b {rd}, {rd}"),
            CSextB(rd) => write!(f, "sext.b {rd}, {rd}"),
            CZextH(rd) => write!(f, "zext.h {rd}, {rd}"),
            CSextH(rd) => write!(f, "sext.h {rd}, {rd}"),
            CNot(rd) => write!(f, "not {rd}, {rd}"),
            CNeg(rd) => write!(f, "neg {rd}, {rd}"),
            CBeq(rs1, rs2, imm) => write!(f, "beq {rs1}, {rs2}, {:#010x}", self.1 + imm),
            CBne(rs1, rs2, imm) => write!(f, "bne {rs1}, {rs2}, {:#010x}", self.1 + imm),
            CAddi2w(rd, rs1, imm) => write!(f, "addi {rd}, {rs1}, {}", imm.0 as i32),
            CAdd2(rd, rs1, rs2) => write!(f, "add {rd}, {rs1}, {rs2}"),
            CSeqz(rd, rs1) => write!(f, "seqz {rd}, {rs1}"),
            CSnez(rd, rs1) => write!(f, "snez {rd}, {rs1}"),
            CAddi2b(rd, rs1, imm) => write!(f, "addi {rd}, {rs1}, {}", imm.0 as i32),
            CSlt(rd, rs1, rs2) => write!(f, "slt {rd}, {rs1}, {rs2}"),
            CSltu(rd, rs1, rs2) => write!(f, "sltu {rd}, {rs1}, {rs2}"),
             _ => write!(f, "{:?}", self.0),
        }
    }
}

impl Instruction {
    pub fn with_pc(&self, pc: Word) -> LocatedInstruction {
        LocatedInstruction(self, pc)
    }
}
