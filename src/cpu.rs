use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::num::Wrapping;
use std::rc::Rc;
use superslice::*;

pub type Word = Wrapping<u32>;

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

pub trait Sra {
    fn sra(&self, shamt: usize) -> Self;
}

impl Sra for Word {
    fn sra(&self, shamt: usize) -> Self {
        (((self.0 as i32) >> shamt) as u32).into_word()
    }
}

pub trait Mulh {
    fn mulh(&self, rhs: &Self) -> Self;
}

impl Mulh for Word {
    fn mulh(&self, rhs: &Self) -> Self {
        ((((self.0 as i32 as i64) * (rhs.0 as i32 as i64)) >> 32) as u32).into_word()
    }
}

pub trait Mulhu {
    fn mulhu(&self, rhs: &Self) -> Self;
}

impl Mulhu for Word {
    fn mulhu(&self, rhs: &Self) -> Self {
        ((((self.0 as i64) * (rhs.0 as i64)) >> 32) as u32).into_word()
    }
}

pub trait Mulhsu {
    fn mulhsu(&self, rhs: &Self) -> Self;
}

impl Mulhsu for Word {
    fn mulhsu(&self, rhs: &Self) -> Self {
        ((((self.0 as i32 as i64) * (rhs.0 as i64)) >> 32) as u32).into_word()
    }
}

pub trait Div {
    fn div(&self, rhs: &Self) -> Self;
}

impl Div for Word {
    fn div(&self, rhs: &Self) -> Self {
        if rhs.0 == 0 {
            (-1).into_word()
        } else if self.0 as i32 == std::i32::MIN && rhs.0 as i32 == -1 {
            std::i32::MIN.into_word()
        } else {
            ((self.0 as i32) / (rhs.0 as i32)).into_word()
        }
    }
}

pub trait Divu {
    fn divu(&self, rhs: &Self) -> Self;
}

impl Divu for Word {
    fn divu(&self, rhs: &Self) -> Self {
        if rhs.0 == 0 {
            std::u32::MAX.into_word()
        } else {
            self / rhs
        }
    }
}

pub trait Rem {
    fn rem(&self, rhs: &Self) -> Self;
}

impl Rem for Word {
    fn rem(&self, rhs: &Self) -> Self {
        if rhs.0 == 0 {
            *self
        } else if self.0 as i32 == std::i32::MIN && rhs.0 as i32 == -1 {
            0.into_word()
        } else {
            ((self.0 as i32) % (rhs.0 as i32)).into_word()
        }
    }
}

pub trait Remu {
    fn remu(&self, rhs: &Self) -> Self;
}

impl Remu for Word {
    fn remu(&self, rhs: &Self) -> Self {
        if rhs.0 == 0 {
            *self
        } else {
            self % rhs
        }
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

pub trait Immediate<T, W>: BitField<T> {
    fn u_imm(&self) -> W;
    fn j_imm(&self) -> W;
    fn i_imm(&self) -> W;
    fn b_imm(&self) -> W;
    fn s_imm(&self) -> W;
    fn ci_imm(&self) -> W;
    fn ciw_uimm(&self) -> W;
    fn clw_uimm(&self) -> W;
    fn csw_uimm(&self) -> W;
    fn cj_imm(&self) -> W;
    fn ci16_imm(&self) -> W;
    fn ciu_imm(&self) -> W;
    fn ci_shamt(&self) -> usize;
    fn cb_imm(&self) -> W;
    fn clwsp_uimm(&self) -> W;
    fn cswsp_uimm(&self) -> W;
    fn csr_uimm(&self) -> W;
}

impl Immediate<u32, Word> for Word {
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
        self.bf(31, 20).into_word().sign_expand(12)
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

    fn ci_imm(&self) -> Word {
        (self.bf(12, 12) << 5 | self.bf(6, 2))
            .into_word()
            .sign_expand(6)
    }

    fn ciw_uimm(&self) -> Word {
        (self.bf(10, 7) << 6 | self.bf(12, 11) << 4 | self.bf(5, 5) << 3 | self.bf(6, 6) << 2)
            .into_word()
            .zero_expand(10)
    }

    fn clw_uimm(&self) -> Word {
        (self.bf(5, 5) << 6 | self.bf(12, 10) << 3 | self.bf(6, 6) << 2)
            .into_word()
            .zero_expand(7)
    }

    fn csw_uimm(&self) -> Word {
        (self.bf(5, 5) << 6 | self.bf(12, 10) << 3 | self.bf(6, 6) << 2)
            .into_word()
            .zero_expand(7)
    }

    fn cj_imm(&self) -> Word {
        (self.bf(12, 12) << 11
            | self.bf(8, 8) << 10
            | self.bf(10, 9) << 8
            | self.bf(6, 6) << 7
            | self.bf(7, 7) << 6
            | self.bf(2, 2) << 5
            | self.bf(11, 11) << 4
            | self.bf(5, 3) << 1)
            .into_word()
            .sign_expand(12)
    }

    fn ci16_imm(&self) -> Word {
        (self.bf(12, 12) << 9
            | self.bf(4, 3) << 7
            | self.bf(5, 5) << 6
            | self.bf(2, 2) << 5
            | self.bf(6, 6) << 4)
            .into_word()
            .sign_expand(10)
    }

    fn ciu_imm(&self) -> Word {
        (self.bf(12, 12) << 17 | self.bf(6, 2) << 12)
            .into_word()
            .sign_expand(18)
    }

    fn ci_shamt(&self) -> usize {
        self.bf(6, 2) as usize
    }

    fn cb_imm(&self) -> Word {
        (self.bf(12, 12) << 8
            | self.bf(6, 5) << 6
            | self.bf(2, 2) << 5
            | self.bf(11, 10) << 3
            | self.bf(4, 3) << 1)
            .into_word()
            .sign_expand(9)
    }

    fn clwsp_uimm(&self) -> Word {
        (self.bf(3, 2) << 6 | self.bf(12, 12) << 5 | self.bf(6, 4) << 2)
            .into_word()
            .zero_expand(8)
    }

    fn cswsp_uimm(&self) -> Word {
        (self.bf(8, 7) << 6 | self.bf(12, 9) << 2)
            .into_word()
            .zero_expand(8)
    }

    fn csr_uimm(&self) -> Word {
        self.bf(19, 15).into_word().zero_expand(5)
    }
}

pub trait InterruptSource {
    fn step_intr(&mut self, _cpu: &Cpu) -> bool;
}

pub trait MemoryMappedIo {
    fn read(&mut self, addr: Word, cpu: &Cpu) -> Word;
    fn write(&mut self, addr: Word, data: Word, cpu: &Cpu);
}

pub struct MemoryMap {
    start: Word,
    len: Word,
    io: Rc<RefCell<dyn MemoryMappedIo>>,
}

impl MemoryMap {
    pub fn new(start: u32, len: u32, io: Rc<RefCell<dyn MemoryMappedIo>>) -> MemoryMap {
        MemoryMap {
            start: start.into_word(),
            len: len.into_word(),
            io,
        }
    }
}

struct MachineTimer {
    time: u64,
    timecmp: u64,
}

impl MachineTimer {
    fn new() -> MachineTimer {
        MachineTimer {
            time: 0,
            timecmp: 0xffff_ffff_ffff_ffffu64,
        }
    }
}

impl MemoryMappedIo for MachineTimer {
    fn read(&mut self, addr: Word, _cpu: &Cpu) -> Word {
        match (addr.0 >> 2) & 3 {
            0 => (self.time as u32).into_word(),
            1 => ((self.time >> 32) as u32).into_word(),
            2 => (self.timecmp as u32).into_word(),
            _ => ((self.timecmp >> 32) as u32).into_word(),
        }
    }

    fn write(&mut self, addr: Word, data: Word, _cpu: &Cpu) {
        match (addr.0 >> 2) & 3 {
            0 => self.time = (self.time & (0xffff_ffff << 32)) | data.0 as u64,
            1 => self.time = (self.time & 0xffff_ffff) | ((data.0 as u64) << 32),
            2 => self.timecmp = (self.timecmp & (0xffff_ffff << 32)) | data.0 as u64,
            _ => self.timecmp = (self.timecmp & 0xffff_ffff) | ((data.0 as u64) << 32),
        }
    }
}

impl InterruptSource for MachineTimer {
    fn step_intr(&mut self, _cpu: &Cpu) -> bool {
        self.time += 1;
        self.time >= self.timecmp
    }
}

pub struct Cpu {
    pub pc: Word,
    pub cycle: u64,
    regs: [Word; 32],
    csr_regs: [Word; 4096],
    machine_timer: Rc<RefCell<MachineTimer>>,
    memory_map: Vec<MemoryMap>,
    interrupt_source: Option<Rc<RefCell<dyn InterruptSource>>>,
    pub ram: Vec<Word>,
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "pc: {:08x}", self.pc)?;
        writeln!(f, "cycle: {:08x}", self.cycle)?;
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
    const RAM_START: u32 = 0x20000000;
    const RAM_SIZE: usize = 0x10000000;
    const RAM_SIZE_IN_WORD: usize = Self::RAM_SIZE >> 2;
    const RAM_MASK_IN_WORD: usize = Self::RAM_SIZE_IN_WORD - 1;
    const RAM_PREFIX_MASK: u32 = !(Self::RAM_SIZE as u32 - 1);

    pub fn new() -> Cpu {
        let mut cpu = Cpu {
            pc: Self::RAM_START.into_word(),
            cycle: 0,
            regs: [0.into_word(); 32],
            csr_regs: [0.into_word(); 4096],
            machine_timer: Rc::new(RefCell::new(MachineTimer::new())),
            memory_map: vec![],
            interrupt_source: Option::None,
            ram: vec![0.into_word(); Self::RAM_SIZE_IN_WORD],
        };
        cpu.memory_map
            .push(MemoryMap::new(0x30002000, 16, cpu.machine_timer.clone()));
        cpu
    }

    pub fn add_memory_map(&mut self, memory_map: MemoryMap) {
        self.memory_map.push(memory_map);
        self.memory_map.sort_by_key(|map| map.start);
    }

    pub fn set_interrupt_source(&mut self, source: Rc<RefCell<dyn InterruptSource>>) {
        self.interrupt_source = Some(source);
    }

    fn inst_read(&self, addr: Word) -> Word {
        let a = (addr >> 2).0 as usize;
        if addr & 2.into_word() != 0.into_word() {
            (self.ram[a & Self::RAM_MASK_IN_WORD] >> 16)
                | ((self.ram[(a + 1) & Self::RAM_MASK_IN_WORD] & 0xffff.into_word()) << 16)
        } else {
            self.ram[a & Self::RAM_MASK_IN_WORD]
        }
    }

    fn find_memory_mapped_io(&self, addr: Word) -> Option<(Rc<RefCell<dyn MemoryMappedIo>>, Word)> {
        let range = self.memory_map.equal_range_by(|map| match map {
            _ if addr < map.start => Ordering::Greater,
            _ if map.start <= addr && addr < map.start + map.len => Ordering::Equal,
            _ => Ordering::Less,
        });
        if range.is_empty() {
            Option::None
        } else {
            let map = &self.memory_map[range.start];
            Option::Some((
                map.io.clone(),
                (addr - map.start) & (map.len - 1.into_word()),
            ))
        }
    }

    fn load_word(&self, addr: Word) -> Word {
        if addr.0 & Self::RAM_PREFIX_MASK == Self::RAM_START {
            self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD]
        } else if let Option::Some((map, addr)) = self.find_memory_mapped_io(addr) {
            map.borrow_mut().read(addr, self)
        } else {
            panic!("invalid address read: {:#010x}", addr.0);
            // 0xdead_beefu32.into_word()
        }
    }

    fn load_subword(&self, addr: Word) -> Word {
        if addr.0 & Self::RAM_PREFIX_MASK == Self::RAM_START {
            self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD] >> ((addr.0 as usize & 3) * 8)
        } else {
            panic!("invalid subword read: {:#010x}", addr.0);
            // 0xdead_beefu32.into_word()
        }
    }

    fn store_word(&mut self, addr: Word, data: Word) {
        if addr.0 & Self::RAM_PREFIX_MASK == Self::RAM_START {
            self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD] = data;
        } else if let Option::Some((map, addr)) = self.find_memory_mapped_io(addr) {
            map.borrow_mut().write(addr, data, self)
        } else {
            panic!("invalid address write: {:#010x}", addr.0);
        }
    }

    fn store_subword(&mut self, addr: Word, data: Word, bits: usize) {
        if addr.0 & Self::RAM_PREFIX_MASK == Self::RAM_START {
            let old = self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD];
            let shift = (addr.0 as usize & 3) * 8;
            let mask = ((1 << bits) - 1).into_word() << shift;
            self.ram[(addr >> 2).0 as usize & Self::RAM_MASK_IN_WORD] =
                (old & !mask) | ((data << shift) & mask);
        } else {
            panic!("invalid subword write: {:#010x}", addr.0);
        }
    }

    fn write_reg(&mut self, rd: u32, val: Word) {
        if rd != 0 {
            self.regs[rd as usize] = val;
        }
    }

    fn read_reg(&self, rs: u32) -> Word {
        self.regs[rs as usize]
    }

    fn csrr<F: FnOnce(Word) -> Word>(&mut self, csr: u32, f: F) -> Word {
        let old = self.csr_regs[csr as usize];
        self.csr_regs[csr as usize] = f(old);
        old
    }

    pub fn step(&mut self) {
        self.cycle += 1;
        self.csr_regs[0xc00] = ((self.cycle & 0xffff_ffff) as u32).into_word();
        self.csr_regs[0xc80] = ((self.cycle >> 32) as u32).into_word();
        self.csr_regs[0xc01] = ((self.cycle & 0xffff_ffff) as u32).into_word();
        self.csr_regs[0xc81] = ((self.cycle >> 32) as u32).into_word();
        let interrupt = |cpu: &mut Cpu, mcause: u32| {
            let mstatus = cpu.csr_regs[0x300];
            cpu.csr_regs[0x300] = ((mstatus & !0x80.into_word())
                | (mstatus.bf(3, 3).into_word() << 7))
                & !8.into_word();
            cpu.csr_regs[0x342] = mcause.into_word();
            cpu.csr_regs[0x341] = cpu.pc;
            cpu.pc = cpu.csr_regs[0x305] & !1.into_word();
        };
        if self
            .interrupt_source
            .as_ref()
            .map_or(false, |s| s.borrow_mut().step_intr(&self))
            && self.csr_regs[0x300].0 & 8 != 0
            && self.csr_regs[0x304].0 & 0x800 != 0
        {
            interrupt(self, 0x8000000bu32);
            return;
        }
        if self.machine_timer.borrow_mut().step_intr(&self)
            && self.csr_regs[0x300].0 & 8 != 0
            && self.csr_regs[0x304].0 & 0x80 != 0
        {
            interrupt(self, 0x80000007u32);
            // println!("timer interrupt!");
            return;
        }
        let inst = self.inst_read(self.pc);
        if inst == 0x00000073.into_word() {
            // ecall
            interrupt(self, 11);
            return;
        }
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
                    // load
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
                        (3, _) => self.write_reg(rd, set_if(data1 < data2)), // sltiu
                        (4, _) => self.write_reg(rd, data1 ^ data2), // xori
                        (6, _) => self.write_reg(rd, data1 | data2), // ori
                        (7, _) => self.write_reg(rd, data1 & data2), // andi
                        (1, 0x00) => self.write_reg(rd, data1 << shamt), // slli
                        (5, 0x00) => self.write_reg(rd, data1 >> shamt), // srli
                        (5, 0x20) => self.write_reg(rd, data1.sra(shamt)), // srai
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
                        (5, 0x20) => self.write_reg(rd, data1.sra(shamt)), // sra
                        (6, 0x00) => self.write_reg(rd, data1 | data2), // or
                        (7, 0x00) => self.write_reg(rd, data1 & data2), // and
                        (0, 0x01) => self.write_reg(rd, data1 * data2), // mul
                        (1, 0x01) => self.write_reg(rd, data1.mulh(&data2)), // mulh
                        (2, 0x01) => self.write_reg(rd, data1.mulhsu(&data2)), // mulhsu
                        (3, 0x01) => self.write_reg(rd, data1.mulhu(&data2)), // mulhu
                        (4, 0x01) => self.write_reg(rd, data1.div(&data2)), // div
                        (5, 0x01) => self.write_reg(rd, data1.divu(&data2)), // divu
                        (6, 0x01) => self.write_reg(rd, data1.rem(&data2)), // rem
                        (7, 0x01) => self.write_reg(rd, data1.remu(&data2)), // remu
                        _ => panic!("unknown alu reg inst"),
                    }
                }
                0x73 => {
                    // csr
                    let rd = inst.bf(11, 7);
                    let rs1 = inst.bf(19, 15);
                    match inst.0 {
                        0x30200073 => {
                            // mret
                            let mstatus = self.csr_regs[0x300];
                            self.csr_regs[0x300] = ((mstatus & !8.into_word())
                                | (mstatus.bf(7, 7).into_word() << 3))
                                | 0x80.into_word();
                            next_pc = self.csr_regs[0x341] & !1.into_word();
                        }
                        _ => {
                            let data1 = self.read_reg(rs1);
                            let uimm1 = inst.csr_uimm();
                            let csr = inst.bf(31, 20);
                            let new_val = match inst.bf(14, 12) {
                                1 => self.csrr(csr, |_| data1),      // csrrw
                                2 => self.csrr(csr, |d| d | data1),  // csrrs
                                3 => self.csrr(csr, |d| d & !data1), // csrrc
                                5 => self.csrr(csr, |_| uimm1),      // csrrwi
                                6 => self.csrr(csr, |d| d | uimm1),  // csrrsi
                                7 => self.csrr(csr, |d| d & !uimm1), // csrrci
                                _ => panic!("unknown csr inst"),
                            };
                            self.write_reg(rd, new_val);
                        }
                    }
                }
                _ => panic!("unknown non-rvc inst"),
            }
        } else {
            // RVC instructions
            next_pc = self.pc + 2.into_word();
            match (opcode & 3, inst.bf(15, 13)) {
                (0, 0) => {
                    // c.addi4spn
                    let rd = inst.bf(4, 2) + 8;
                    self.write_reg(rd, self.read_reg(2) + inst.ciw_uimm());
                }
                (0, 2) => {
                    // c.lw
                    let rd = inst.bf(4, 2) + 8;
                    let rs = inst.bf(9, 7) + 8;
                    self.write_reg(rd, self.load_word(self.read_reg(rs) + inst.clw_uimm()));
                }
                (0, 6) => {
                    // c.sw
                    let rs1 = inst.bf(9, 7) + 8;
                    let rs2 = inst.bf(4, 2) + 8;
                    self.store_word(self.read_reg(rs1) + inst.csw_uimm(), self.read_reg(rs2));
                }
                (1, 0) => {
                    // c.nop / c.addi
                    let rd = inst.bf(11, 7);
                    self.write_reg(rd, self.read_reg(rd) + inst.ci_imm());
                }
                (1, 1) => {
                    // c.jal
                    self.write_reg(1, next_pc);
                    next_pc = self.pc + inst.cj_imm();
                }
                (1, 2) => self.write_reg(inst.bf(11, 7), inst.ci_imm()), // c.li
                (1, 3) => {
                    let rd = inst.bf(11, 7);
                    match rd {
                        0 => panic!("unknown rvc li inst"),
                        2 => self.write_reg(rd, self.read_reg(rd) + inst.ci16_imm()), // c.addi16sp
                        _ => self.write_reg(rd, inst.ciu_imm()),                      // c.lui
                    }
                }
                (1, 4) => {
                    let rd = inst.bf(9, 7) + 8;
                    let rs = inst.bf(4, 2) + 8;
                    match (inst.bf(11, 10), inst.bf(6, 5), inst.bf(12, 12)) {
                        (0, _, _) => self.write_reg(rd, self.read_reg(rd) >> inst.ci_shamt()), // c.srli
                        (1, _, _) => self.write_reg(rd, self.read_reg(rd).sra(inst.ci_shamt())), // c.srai
                        (2, _, _) => self.write_reg(rd, self.read_reg(rd) & inst.ci_imm()), // c.andi
                        (3, 0, 0) => self.write_reg(rd, self.read_reg(rd) - self.read_reg(rs)), // c.sub
                        (3, 1, 0) => self.write_reg(rd, self.read_reg(rd) ^ self.read_reg(rs)), // c.xor
                        (3, 2, 0) => self.write_reg(rd, self.read_reg(rd) | self.read_reg(rs)), // c.or
                        (3, 3, 0) => self.write_reg(rd, self.read_reg(rd) & self.read_reg(rs)), // c.and
                        _ => panic!("unknown rvc arithmetic inst"),
                    }
                }
                (1, 5) => next_pc = self.pc + inst.cj_imm(), // c.j
                (1, 6) => {
                    // c.beqz
                    let rs = inst.bf(9, 7) + 8;
                    if self.read_reg(rs) == 0.into_word() {
                        next_pc = self.pc + inst.cb_imm();
                    }
                }
                (1, 7) => {
                    // c.bnez
                    let rs = inst.bf(9, 7) + 8;
                    if self.read_reg(rs) != 0.into_word() {
                        next_pc = self.pc + inst.cb_imm();
                    }
                }
                (2, 0) => {
                    // c.slli
                    let rd = inst.bf(11, 7);
                    self.write_reg(rd, self.read_reg(rd) << inst.ci_shamt());
                }
                (2, 2) => {
                    // c.lwsp
                    let rd = inst.bf(11, 7);
                    self.write_reg(rd, self.load_word(self.read_reg(2) + inst.clwsp_uimm()));
                }
                (2, 4) => {
                    match (inst.bf(12, 12), inst.bf(11, 7), inst.bf(6, 2)) {
                        (0, rs, 0) if rs != 0 => next_pc = self.read_reg(rs) & !1.into_word(), // c.jr
                        (0, rd, rs) if rd != 0 => self.write_reg(rd, self.read_reg(rs)), // c.mv
                        (1, 0, 0) => (),                                                 // c.ebreak
                        (1, rs, 0) => {
                            // c.jalr
                            self.write_reg(1, next_pc);
                            next_pc = self.read_reg(rs) & !1.into_word();
                        }
                        (1, rd, rs) => self.write_reg(rd, self.read_reg(rd) + self.read_reg(rs)), // c.add
                        _ => panic!("unexpected rvc reg inst"),
                    }
                }
                (2, 6) => {
                    // c.swsp
                    let rs = inst.bf(6, 2);
                    self.store_word(self.read_reg(2) + inst.cswsp_uimm(), self.read_reg(rs));
                }
                _ => panic!("unknown rvc inst"),
            }
        }
        self.pc = next_pc;
    }

    pub fn next_inst(&self) -> Word {
        let inst = self.inst_read(self.pc);
        if inst.bf(1, 0) == 3 {
            inst
        } else {
            inst.bf(15, 0).into_word()
        }
    }
}
