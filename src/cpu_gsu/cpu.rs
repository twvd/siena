use anyhow::Result;
use serde::{Deserialize, Serialize};

use super::regs::{CFGRFlag, Flag, PORFlag, Register, RegisterFile, ScreenHeight, BPP};

use crate::tickable::{Tickable, Ticks};

pub type GsuAddress = u32;
pub const GSU_ADDRESS_MASK: GsuAddress = 0xFFFFFF;
fn gsu_addr_add(addr: GsuAddress, i: GsuAddress) -> GsuAddress {
    (addr & 0xFF0000) | (addr.wrapping_add(i) & 0xFFFF)
}

pub const CACHE_LINES: usize = 32;
pub const CACHE_LINE_SIZE: usize = 16;
pub const CACHE_SIZE: usize = CACHE_LINES * CACHE_LINE_SIZE;

#[derive(Serialize, Deserialize)]
pub enum GsuBus {
    ROM,
    RAM,
    Cache,
}

/// SuperFX CPU (GSU)
#[derive(Serialize, Deserialize)]
pub struct CpuGsu {
    pub regs: RegisterFile,
    pub cycles: Ticks,
    pub cache: Vec<u8>,
    pub rom: Vec<u8>,
    pub ram: Vec<u8>,

    sreg: usize,
    dreg: usize,
    last_instr: u8,
    branch_pc: Option<u16>,
    irq_pending: bool,

    pub cache_valid: [bool; CACHE_LINES],
}

impl CpuGsu {
    pub fn new(rom: &[u8]) -> Self {
        let mut c = Self {
            regs: RegisterFile::new(),
            cycles: 0,
            cache: vec![0; CACHE_SIZE],
            rom: vec![0xFF; 8 * 1024 * 1024],
            ram: vec![0xFF; 256 * 1024],
            sreg: 0,
            dreg: 0,
            last_instr: 0,
            branch_pc: None,
            cache_valid: [false; CACHE_LINES],
            irq_pending: false,
        };

        c.rom[0..rom.len()].copy_from_slice(rom);
        c
    }

    pub fn cache_flush(&mut self) {
        self.cache_valid = [false; CACHE_LINES];
    }

    pub fn get_cache_base(&self) -> GsuAddress {
        (GsuAddress::from(self.regs.read(Register::PBR)) << 16)
            | GsuAddress::from(self.regs.read(Register::CBR))
    }

    pub fn determine_bus(&self, fulladdr: GsuAddress) -> GsuBus {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        // Check cache
        let cache_base = self.get_cache_base() as usize;
        if (cache_base..=(cache_base + CACHE_SIZE)).contains(&(fulladdr as usize)) {
            let cache_line = ((fulladdr as usize) - cache_base) / CACHE_LINE_SIZE;
            if self.cache_valid[cache_line] {
                return GsuBus::Cache;
            }
        }

        match (bank & !0x80, addr) {
            (0x00..=0x3F, 0x8000..=0xFFFF) => GsuBus::ROM,
            (0x40..=0x5F, _) => GsuBus::ROM,
            (0x70..=0x71, _) => GsuBus::RAM,
            _ => panic!("Unmapped address {:06X}", fulladdr),
        }
    }

    pub fn read_bus(&self, fulladdr: GsuAddress) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        // Check cache. Note: get_cache_base() includes PBR
        let cache_base = self.get_cache_base() as usize;
        if (cache_base..=(cache_base + CACHE_SIZE)).contains(&(fulladdr as usize)) {
            let cache_line = (fulladdr as usize - cache_base) / CACHE_LINE_SIZE;
            let cache_line_pos = (fulladdr as usize - cache_base) % CACHE_LINE_SIZE;
            if self.cache_valid[cache_line] {
                return self.cache[(cache_line * CACHE_LINE_SIZE) + cache_line_pos];
            }
        }

        match (bank & !0x80, addr) {
            (0x00..=0x3F, 0x0000..=0x7FFF) => self.rom[addr + bank * 0x8000],
            (0x00..=0x3F, 0x8000..=0xFFFF) => self.rom[addr - 0x8000 + bank * 0x8000],
            (0x40..=0x5F, _) => self.rom[(bank - 0x40) * 0x10000 + addr],
            (0x70..=0x71, _) => self.ram[(bank - 0x70) * 0x10000 + addr],
            _ => panic!("Unmapped address {:06X}", fulladdr),
        }
    }

    pub fn read16_bus(&self, fulladdr: GsuAddress) -> u16 {
        (self.read_bus(fulladdr) as u16) | ((self.read_bus(gsu_addr_add(fulladdr, 1)) as u16) << 8)
    }

    fn read_bus_tick(&self, fulladdr: GsuAddress) -> u8 {
        // TODO bus access clear check
        // TODO wait states based on location

        self.read_bus(fulladdr)
    }

    pub fn read16_bus_tick(&self, fulladdr: GsuAddress) -> u16 {
        (self.read_bus_tick(fulladdr) as u16)
            | ((self.read_bus_tick(gsu_addr_add(fulladdr, 1)) as u16) << 8)
    }

    fn fetch(&mut self) -> u8 {
        let pc_bank = GsuAddress::from(self.regs.read(Register::PBR)) << 16;
        let pc = pc_bank | GsuAddress::from(self.regs.read_inc(Register::R15));

        if let Some(branch_pc) = self.branch_pc {
            // Branch scheduled, now in delay slot
            self.regs.write(Register::R15, branch_pc);
            self.branch_pc = None;
        }

        let cache_base = self.get_cache_base() as usize;
        if (cache_base..=(cache_base + CACHE_SIZE)).contains(&(pc as usize)) {
            // PC in cache region
            let cache_line = (pc as usize - cache_base) / CACHE_LINE_SIZE;
            let cache_line_pos = (pc as usize - cache_base) % CACHE_LINE_SIZE;
            let cache_pos = pc as usize - cache_base;
            if !self.cache_valid[cache_line] {
                // Cache miss, load to cache
                self.cache[cache_pos] = self.read_bus_tick(pc);
                if cache_line_pos == (CACHE_LINE_SIZE - 1) {
                    // Mark cache line valid
                    self.cache_valid[cache_line] = true;
                }
            }

            self.cache[cache_pos]
        } else {
            self.read_bus_tick(pc)
        }
    }

    fn fetch16(&mut self) -> u16 {
        let lo = self.fetch() as u16;
        let hi = self.fetch() as u16;
        lo | (hi << 8)
    }

    fn alu_sub(&mut self, a: u16, b: u16, c: u16) -> u16 {
        let result = i32::from(a) + i32::from(!b) + i32::from(c);
        self.regs.write_flags(&[
            (Flag::Z, (result as u16) == 0),
            (Flag::S, result & 0x8000 != 0),
            (Flag::C, result > u16::MAX.into()),
            (Flag::V, !(a ^ !b) & (a ^ result as u16) & 0x8000 != 0),
        ]);
        result as u16
    }

    fn alu_add(&mut self, a: u16, b: u16, c: u16) -> u16 {
        let result = u32::from(a) + u32::from(b) + u32::from(c);
        self.regs.write_flags(&[
            (Flag::Z, (result as u16) == 0),
            (Flag::S, result & 0x8000 != 0),
            (Flag::C, result > u16::MAX.into()),
            (Flag::V, !(a ^ b) & (a ^ result as u16) & 0x8000 != 0),
        ]);
        result as u16
    }

    pub fn step(&mut self) -> Result<()> {
        let instr = self.fetch();

        // Note: ALTx is ignored if the opcode following does not
        // require ALTx.
        let alt1 = self.regs.test_flag(Flag::ALT1);
        let alt2 = self.regs.test_flag(Flag::ALT2);
        self.regs
            .write_flags(&[(Flag::ALT1, false), (Flag::ALT2, false)]);

        let sreg = self.sreg;
        let dreg = self.dreg;
        // SREG/DREG are reset after execution, but should persist
        // for branch instructions.
        self.sreg = 0;
        self.dreg = 0;

        let last_instr = self.last_instr;
        self.last_instr = instr;

        match (instr, alt1, alt2) {
            (0x00, _, _) => {
                // STOP
                self.regs.write_flags(&[(Flag::G, false)]);
                println!("{}", self.regs);

                if !self.regs.test_cfgr(CFGRFlag::IRQ) {
                    self.regs.write_flags(&[(Flag::IRQ, true)]);
                    self.irq_pending = true;
                }

                self.cycles(1)?;
            }
            (0x01, _, _) => {
                // NOP
                self.cycles(1)?;
            }
            (0x02, _, _) => {
                // CACHE
                let pc = self.regs.read(Register::R15);
                let cbr = self.regs.read(Register::CBR);
                if (pc & 0xFFF0) != cbr {
                    self.cache_flush();
                    self.regs.write(Register::CBR, pc & 0xFFF0);
                }
                self.cycles(1)?;
            }
            (0x03, _, _) => {
                // LSR
                let s = self.regs.read_r(sreg);
                let result = s >> 1;
                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::S, false),
                    (Flag::C, s & 0x01 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x04, _, _) => {
                // ROL
                let s = self.regs.read_r(sreg);
                let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };
                let result = s << 1 | c;

                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::S, result & 0x8000 != 0),
                    (Flag::C, s & 0x8000 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x05, _, _) => {
                // BRA
                self.op_branch(true)
            }
            (0x06, _, _) => {
                // BGE
                self.op_branch(!(self.regs.test_flag(Flag::S) ^ self.regs.test_flag(Flag::V)))
            }
            (0x07, _, _) => {
                // BLT
                self.op_branch(self.regs.test_flag(Flag::S) ^ self.regs.test_flag(Flag::V))
            }
            (0x08, _, _) => {
                // BNE
                self.op_branch(!self.regs.test_flag(Flag::Z))
            }
            (0x09, _, _) => {
                // BEQ
                self.op_branch(self.regs.test_flag(Flag::Z))
            }
            (0x0A, _, _) => {
                // BPL
                self.op_branch(!self.regs.test_flag(Flag::S))
            }
            (0x0B, _, _) => {
                // BMI
                self.op_branch(self.regs.test_flag(Flag::S))
            }
            (0x0C, _, _) => {
                // BCC
                self.op_branch(!self.regs.test_flag(Flag::C))
            }
            (0x0D, _, _) => {
                // BCS
                self.op_branch(self.regs.test_flag(Flag::C))
            }
            (0x0E, _, _) => {
                // BVC
                self.op_branch(!self.regs.test_flag(Flag::V))
            }
            (0x0F, _, _) => {
                // BVS
                self.op_branch(self.regs.test_flag(Flag::V))
            }
            (0x10..=0x1F, _, _) => {
                // MOVE/TO
                let reg = (instr & 0x0F) as usize;

                if last_instr & 0xF0 == 0x20 {
                    // MOVE
                    let val = self.regs.read_r(sreg);
                    self.regs.write_r(reg, val);
                } else {
                    // TO
                    self.sreg = sreg;
                    self.dreg = reg;
                }

                self.cycles(1)?;
            }
            (0x20..=0x2F, _, _) => {
                // WITH
                let reg = (instr & 0x0F) as usize;
                self.sreg = reg;
                self.dreg = reg;
                // cycles unknown, assumed 3/3/1
                self.cycles(1)?;
            }
            (0x30..=0x3B, false, _) => {
                // STW (Rn)
                let addr = (usize::from(self.regs.read(Register::RAMBR)) << 8)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize));
                let v = self.regs.read_r(sreg);
                self.ram[addr] = v as u8;
                self.ram[addr + 1] = (v >> 8) as u8;
                self.cycles(1)?;
            }
            (0x30..=0x3B, true, _) => {
                // STB (Rn)
                let addr = (usize::from(self.regs.read(Register::RAMBR)) << 8)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize));
                let v = self.regs.read_r(sreg);
                self.ram[addr] = v as u8;
                self.cycles(1)?;
            }
            (0x3C, _, _) => {
                // LOOP
                let i = self.regs.read(Register::R12);
                let new_i = i.wrapping_sub(1);
                self.regs.write(Register::R12, new_i);
                self.regs
                    .write_flags(&[(Flag::S, new_i & 0x8000 != 0), (Flag::Z, new_i == 0)]);
                if new_i != 0 {
                    let new_pc = self.regs.read(Register::R13);
                    self.branch_pc = Some(new_pc);
                }
                self.cycles(1)?;
            }
            (0x3D, _, _) => {
                // ALT1
                self.regs.write_flags(&[(Flag::ALT1, true)]);

                // Prefix opcodes preserve SReg/DReg
                self.sreg = sreg;
                self.dreg = dreg;

                self.cycles(1)?;
            }
            (0x3E, _, _) => {
                // ALT2
                self.regs.write_flags(&[(Flag::ALT2, true)]);

                // Prefix opcodes preserve SReg/DReg
                self.sreg = sreg;
                self.dreg = dreg;

                self.cycles(1)?;
            }
            (0x3F, _, _) => {
                // ALT3
                self.regs
                    .write_flags(&[(Flag::ALT1, true), (Flag::ALT2, true)]);

                // Prefix opcodes preserve SReg/DReg
                self.sreg = sreg;
                self.dreg = dreg;

                self.cycles(1)?;
            }
            (0x40..=0x4B, false, _) => {
                // LDW (Rn)
                let addr_l = (usize::from(self.regs.read(Register::RAMBR)) << 8)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize));
                let addr_h = (usize::from(self.regs.read(Register::RAMBR)) << 8)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize).wrapping_add(1));
                let v = self.ram[addr_l] as u16 | ((self.ram[addr_h] as u16) << 8);
                self.regs.write_r(dreg, v);
                self.cycles(7)?;
            }
            (0x40..=0x4B, true, _) => {
                // LDB (Rn)
                let addr_l = (usize::from(self.regs.read(Register::RAMBR)) << 8)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize));
                let v = self.ram[addr_l] as u16;
                self.regs.write_r(dreg, v);
                self.cycles(6)?;
            }
            (0x4C, false, _) => {
                // PLOT
                self.pixel_draw();
                let _ = self.regs.read_inc(Register::R1);
                self.cycles(1)?;
            }
            (0x4C, true, _) => {
                // RPIX
                // TODO pixel cache
            }
            (0x4D, false, _) => {
                // SWAP
                let s = self.regs.read_r(sreg);
                let result = s.rotate_right(8);

                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x4E, false, _) => {
                // COLOR
                let s = self.regs.read_r(sreg) & 0xFF;
                self.regs.write(Register::COLR, s);
                self.cycles(1)?;
            }
            (0x4E, true, _) => {
                // CMODE
                let s = self.regs.read_r(sreg) & 0x1F;
                self.regs.write(Register::POR, s);
                self.cycles(1)?;
            }
            (0x4F, _, _) => {
                // NOT
                let result = !self.regs.read_r(sreg);
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x50..=0x5F, false, false) => {
                // ADD Rn
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);

                let result = self.alu_add(s1, s2, 0);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x50..=0x5F, true, false) => {
                // ADC Rn
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);
                let c = if self.regs.test_flag(Flag::C) {
                    1_u16
                } else {
                    0_u16
                };

                let result = self.alu_add(s1, s2, c);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x50..=0x5F, false, true) => {
                // ADD #n
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;

                let result = self.alu_add(s1, s2, 0);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x50..=0x5F, true, true) => {
                // ADC #n
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;
                let c = if self.regs.test_flag(Flag::C) {
                    1_u16
                } else {
                    0_u16
                };

                let result = self.alu_add(s1, s2, c);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x60..=0x6F, false, false) => {
                // SUB Rn
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);

                let result = self.alu_sub(s1, s2, 1);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x60..=0x6F, true, false) => {
                // SBC Rn
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);
                let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };

                let result = self.alu_sub(s1, s2, c);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x60..=0x6F, false, true) => {
                // SUB #n
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;

                let result = self.alu_sub(s1, s2, 1);
                self.regs.write_r(dreg, result);
                self.cycles(1)?;
            }
            (0x60..=0x6F, true, true) => {
                // CMP
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r((instr & 0x0F) as usize);

                let _ = self.alu_sub(s1, s2, 1);
                self.cycles(1)?;
            }
            (0x70, _, _) => {
                // MERGE
                let result = (self.regs.read(Register::R7) & 0xFF00)
                    .wrapping_add(self.regs.read(Register::R8) / 0x0100);
                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::S, result & 0x8080 != 0),
                    (Flag::V, result & 0xC0C0 != 0),
                    (Flag::C, result & 0xE0E0 != 0),
                    (Flag::Z, result & 0xF0F0 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x71..=0x7F, false, false) => {
                // AND r#
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);

                let result = s1 & s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x71..=0x7F, true, false) => {
                // BIC r#
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);

                let result = s1 & !s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x71..=0x7F, false, true) => {
                // AND #
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;

                let result = s1 & s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x71..=0x7F, true, true) => {
                // BIC #
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;

                let result = s1 & !s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x80..=0x8F, false, _) => {
                // MULT Rn / #n
                let a = self.regs.read_r(sreg) as i8 as u16;
                let b = if !alt2 {
                    self.regs.read_r((instr & 0x0F) as usize) as i8
                } else {
                    (instr & 0x0F) as i8
                } as u16;

                let result = a.wrapping_mul(b);
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);

                if self.regs.test_cfgr(CFGRFlag::MS0) {
                    self.cycles(1)?;
                } else {
                    self.cycles(2)?;
                }
            }
            (0x80..=0x8F, true, _) => {
                // UMULT Rn / #n
                let a = self.regs.read_r(sreg) as u8 as u16;
                let b = if !alt2 {
                    self.regs.read_r((instr & 0x0F) as usize) as u8
                } else {
                    (instr & 0x0F) as u8
                } as u16;

                let result = a.wrapping_mul(b);
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);

                if self.regs.test_cfgr(CFGRFlag::MS0) {
                    self.cycles(1)?;
                } else {
                    self.cycles(2)?;
                }
            }
            (0x91..=0x94, _, _) => {
                // LINK #n
                let v = self
                    .regs
                    .read(Register::R15)
                    .wrapping_add((instr & 0x0F) as u16);
                self.regs.write(Register::R11, v);
                self.cycles(1)?;
            }
            (0x95, _, _) => {
                // SEX
                let s = self.regs.read_r(sreg) & 0xFF;
                let sgn = if s & 0x80 != 0 { 0xFF00_u16 } else { 0_u16 };
                let result = sgn | s;

                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x96, false, _) => {
                // ASR
                let s = self.regs.read_r(sreg);
                let result = ((s as i16) >> 1) as u16;

                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::S, result & 0x8000 != 0),
                    (Flag::C, s & 0x01 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x96, true, _) => {
                // DIV2
                let s = self.regs.read_r(sreg);
                let result = if s == 0xFFFF {
                    0
                } else {
                    ((s as i16) >> 1) as u16
                };

                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::S, result & 0x8000 != 0),
                    (Flag::C, s & 0x01 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x97, _, _) => {
                // ROR
                let s = self.regs.read_r(sreg);
                let c = if self.regs.test_flag(Flag::C) {
                    0x8000
                } else {
                    0
                };
                let result = s >> 1 | c;

                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::S, result & 0x8000 != 0),
                    (Flag::C, s & 0x01 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x98..=0x9D, false, _) => {
                // JMP Rn
                let r = (instr & 0x0F) as usize;
                self.branch_pc = Some(self.regs.read_r(r));
                self.cycles(1)?;
            }
            (0x9E, _, _) => {
                // LOB
                let result = self.regs.read_r(sreg) & 0xFF;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x80 != 0)]);
                self.cycles(1)?;
            }
            (0x9F, _, false) => {
                // FMULT/LMULT
                let a = self.regs.read_r(sreg) as i16 as i32;
                let b = self.regs.read(Register::R6) as i16 as i32;
                let result = (a * b) as u32;

                if alt1 {
                    // LMULT
                    self.regs.write(Register::R4, result as u16);
                    self.cycles(1)?;
                }

                // If LMULT is called with DReg = R4, R4 will contain MSB
                self.regs.write_r(dreg, (result >> 16) as u16);
                self.regs.write_flags(&[
                    (Flag::Z, (result >> 16) == 0),
                    (Flag::S, result & 0x80000000 != 0),
                    (Flag::C, result & 0x8000 != 0),
                ]);

                if self.regs.test_cfgr(CFGRFlag::MS0) {
                    self.cycles(4)?;
                } else {
                    self.cycles(8)?;
                }
            }
            (0xA0..=0xAF, _, _) => {
                // IBT Rn,imm
                let reg = (instr & 0x0F) as usize;
                let s = self.fetch() as u16;
                let sgn = if s & 0x80 != 0 { 0xFF00_u16 } else { 0_u16 };
                let result = sgn | s;

                self.regs.write_r(reg, result);
                self.cycles(2)?;
            }
            (0xB0..=0xBF, _, _) => {
                // FROM/MOVES
                let reg = (instr & 0x0F) as usize;

                if last_instr & 0xF0 == 0x20 {
                    // MOVES
                    let val = self.regs.read_r(reg);
                    self.regs.write_r(sreg, val);
                    self.regs.write_flags(&[
                        (Flag::S, val & 0x8000 != 0),
                        (Flag::Z, val == 0),
                        (Flag::V, val & 0x80 != 0),
                    ]);
                } else {
                    // FROM
                    self.sreg = reg;
                    self.dreg = dreg;
                }
                self.cycles(1)?;
            }
            (0xC0, _, _) => {
                // HIB
                let result = self.regs.read_r(sreg) >> 8;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x80 != 0)]);
                self.cycles(1)?;
            }
            (0xC1..=0xCF, false, false) => {
                // OR r#
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);

                let result = s1 | s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xC1..=0xCF, true, false) => {
                // XOR r#
                let s2reg = (instr & 0x0F) as usize;
                let s1 = self.regs.read_r(sreg);
                let s2 = self.regs.read_r(s2reg);

                let result = s1 ^ s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xC1..=0xCF, false, true) => {
                // OR #
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;

                let result = s1 | s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xC1..=0xCF, true, true) => {
                // XOR #
                let s1 = self.regs.read_r(sreg);
                let s2 = (instr & 0x0F) as u16;

                let result = s1 ^ s2;
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xD0..=0xDE, _, _) => {
                // INC Rn
                let reg = (instr & 0x0F) as usize;
                let result = self.regs.read_r(reg).wrapping_add(1);
                self.regs.write_r(reg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xDF, _, _) => {
                // GETC
                let addr = (GsuAddress::from(self.regs.read(Register::ROMBR)) << 16)
                    | GsuAddress::from(self.regs.read(Register::R14));
                // TODO ROM cache
                let s = self.read_bus_tick(addr);
                self.regs.write8(Register::COLR, s);
                self.cycles(1)?;
            }
            (0xE0..=0xEE, _, _) => {
                // DEC Rn
                let reg = (instr & 0x0F) as usize;
                let result = self.regs.read_r(reg).wrapping_sub(1);
                self.regs.write_r(reg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xEF, _, _) => {
                // GETBx
                let addr = (GsuAddress::from(self.regs.read(Register::ROMBR)) << 16)
                    | GsuAddress::from(self.regs.read(Register::R14));
                // TODO ROM cache
                let val = self.read_bus_tick(addr) as u16;
                let d = self.regs.read_r(dreg);
                self.regs.write_r(
                    dreg,
                    match (alt1, alt2) {
                        // GETB
                        (false, false) => val,
                        // GETBH
                        (true, false) => (val << 8) | (d & 0x00FF),
                        // GETBL
                        (false, true) => val | (val & 0xFF00),
                        // GETBS
                        (true, true) => {
                            if val & 0x80 != 0 {
                                val | 0xFF00
                            } else {
                                val
                            }
                        }
                    },
                );
                self.cycles(1)?;
            }
            (0xF0..=0xFF, _, _) => {
                // IWT
                let reg = (instr & 0x0F) as usize;
                let imm = self.fetch16();
                self.regs.write_r(reg, imm);
                self.cycles(3)?;
            }
            _ => panic!(
                "Unimplemented instruction {:02X} alt1 = {} alt2 = {}",
                instr, alt1, alt2
            ),
        }
        Ok(())
    }

    fn cycles(&mut self, cycles: Ticks) -> Result<()> {
        // TODO cycles

        Ok(())
    }

    fn pixel_draw(&mut self) {
        let x = self.regs.read(Register::R1) as usize;
        let y = self.regs.read(Register::R2) as usize;
        let color = self.regs.read(Register::COLR) as usize;

        // Transparency
        if !self.regs.test_por(PORFlag::NotTransparent) && color == 0 {
            return;
        }

        let bpp = self.regs.get_scmr_bpp();
        let tilenum = if self.regs.get_scmr_height() == ScreenHeight::Obj
            || self.regs.test_por(PORFlag::ObjMode)
        {
            todo!();
        } else {
            match self.regs.get_scmr_height() {
                ScreenHeight::H128 => (x / 8) * 0x10 + (y / 8),
                ScreenHeight::H160 => (x / 8) * 0x14 + (y / 8),
                ScreenHeight::H192 => (x / 8) * 0x18 + (y / 8),
                ScreenHeight::Obj => unreachable!(),
            }
        };
        let scbr = self.regs.read(Register::SCBR) as usize;
        let row_addr = match bpp {
            BPP::Two => tilenum * 0x10 + scbr * 0x400 + (y & 7) * 2,
            BPP::Four => tilenum * 0x20 + scbr * 0x400 + (y & 7) * 2,
            BPP::Eight => tilenum * 0x40 + scbr * 0x400 + (y & 7) * 2,
        };

        for bitp in 0..(bpp.num_bitplanes()) {
            if color & (1 << bitp) != 0 {
                self.ram[row_addr + bitp] |= 1 << (7 - (x % 8));
            }
        }
    }

    fn op_branch(&mut self, cond: bool) {
        let dest = self.fetch() as i8 as i16;
        let pc = self.regs.read(Register::R15);
        let new_pc = pc.wrapping_add_signed(dest);

        if cond {
            self.branch_pc = Some(new_pc);
        }
        // TODO cycles
    }

    pub fn get_clr_int(&mut self) -> bool {
        let v = self.irq_pending;
        self.irq_pending = false;
        v
    }
}

impl Tickable for CpuGsu {
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        if !self.regs.test_flag(Flag::G) {
            // GSU stopped
            return Ok(());
        }

        // TODO credits
        self.step()
    }
}
