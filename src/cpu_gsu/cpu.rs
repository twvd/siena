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

#[derive(Debug, Serialize, Deserialize)]
pub enum GsuMap {
    SuperFX1,
    SuperFX2,
}

#[derive(Serialize, Deserialize)]
pub enum GsuBus {
    ROM,
    RAM,
    Cache,
}

/// SuperFX CPU (GSU)
#[derive(Serialize, Deserialize)]
pub struct CpuGsu {
    pub verbose: bool,
    pub regs: RegisterFile,
    pub cycles: Ticks,
    pub rom: Vec<u8>,
    pub ram: Vec<u8>,
    pub bram: Vec<u8>,

    pub sreg: usize,
    pub dreg: usize,
    last_ramaddr: u16,

    /// Cache
    pub cache: Vec<u8>,

    /// Cache valid markers (per cache line)
    pub cache_valid: [bool; CACHE_LINES],

    rom_buffer: u8,

    // Memory map
    map: GsuMap,
    pub ram_mask: usize,
    pub rom_mask: usize,

    pixelcache_x: usize,
    pixelcache_y: usize,
    pixelcache_drawn: u8,
}

impl CpuGsu {
    /// Offsets of color bitplanes in a single tile pixel
    const BITPLANE_OFFSETS: [usize; 8] = [0x00, 0x01, 0x10, 0x11, 0x20, 0x21, 0x30, 0x31];

    pub fn new(rom: &[u8], map: GsuMap, ram_mask: usize) -> Self {
        let mut c = Self {
            verbose: false,
            regs: RegisterFile::new(),
            cycles: 0,
            cache: vec![0; CACHE_SIZE],
            rom: vec![0xFF; 8 * 1024 * 1024],
            ram: vec![0xFF; 0x20000],
            bram: vec![0xFF; 128 * 1024],
            sreg: 0,
            dreg: 0,
            last_ramaddr: 0,
            cache_valid: [false; CACHE_LINES],
            rom_buffer: 0,
            map,
            ram_mask,
            rom_mask: (rom.len() - 1),
            pixelcache_x: 0,
            pixelcache_y: 0,
            pixelcache_drawn: 0,
        };

        c.rom[0..rom.len()].copy_from_slice(rom);
        c
    }

    /// Invalidates the entire cache
    pub fn cache_flush(&mut self) {
        self.cache_valid = [false; CACHE_LINES];
    }

    /// Returns the current cache base address, including bank
    pub fn get_cache_base(&self) -> GsuAddress {
        (GsuAddress::from(self.regs.read(Register::PBR)) << 16)
            | GsuAddress::from(self.regs.read(Register::CBR))
    }

    /// Returns the cache line the specified address lives in or
    /// None if it is outside the cache region.
    fn get_addr_cache_line(&self, addr: GsuAddress) -> Option<usize> {
        let cache_base = self.get_cache_base() as usize;
        if (cache_base..(cache_base + CACHE_SIZE)).contains(&(addr as usize)) {
            return Some(((addr as usize) - cache_base) / CACHE_LINE_SIZE);
        }
        None
    }

    /// Reads a byte from the cache or None if address is not cached.
    fn read_cache(&mut self, addr: GsuAddress) -> Option<u8> {
        if let Some(cache_line) = self.get_addr_cache_line(addr) {
            let cache_line_pos = (addr as usize) % CACHE_LINE_SIZE;
            let cache_line_offset = cache_line * CACHE_LINE_SIZE;

            if !self.cache_valid[cache_line] {
                // Fill cache line now
                for i in cache_line_offset..(cache_line_offset + CACHE_LINE_SIZE) {
                    self.cache[i] = self.read_bus_tick(self.get_cache_base() + (i as GsuAddress));
                }
                self.cache_valid[cache_line] = true;
            }
            return Some(self.cache[cache_line_offset + cache_line_pos]);
        }
        None
    }

    pub fn determine_bus(&self, fulladdr: GsuAddress) -> GsuBus {
        if let Some(cache_line) = self.get_addr_cache_line(fulladdr) {
            if self.cache_valid[cache_line] {
                return GsuBus::Cache;
            }
        }

        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match self.map {
            GsuMap::SuperFX1 => match (bank & !0x80, addr) {
                (0x00..=0x3F, 0x8000..=0xFFFF) => GsuBus::ROM,
                (0x40..=0x5F, _) => GsuBus::ROM,
                (0x70..=0x71, _) => GsuBus::RAM,
                (0x78, _) => GsuBus::RAM,
                _ => GsuBus::Cache, // ? open bus
            },
            GsuMap::SuperFX2 => match (bank & !0x80, addr) {
                (0x00..=0x3F, _) => GsuBus::ROM,
                (0x40..=0x5F, _) => GsuBus::ROM,
                (0x70..=0x71, _) => GsuBus::RAM,
                _ => GsuBus::Cache, // ? open bus
            },
        }
    }

    pub fn read_bus(&self, fulladdr: GsuAddress) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        let bank = bank & !0x80;
        match self.map {
            GsuMap::SuperFX1 => match (bank, addr) {
                (0x00..=0x3F, 0x0000..=0x7FFF) => self.rom[(addr + bank * 0x8000) & self.rom_mask],
                (0x00..=0x3F, 0x8000..=0xFFFF) => {
                    self.rom[(addr - 0x8000 + bank * 0x8000) & self.rom_mask]
                }
                (0x40..=0x5F, _) => self.rom[((bank - 0x40) * 0x10000 + addr) & self.rom_mask],
                (0x70..=0x71, _) => self.ram[((bank - 0x70) * 0x10000 + addr) & self.ram_mask],
                _ => {
                    println!("GSU reading unmapped address: {:06X}", fulladdr);
                    0
                }
            },
            GsuMap::SuperFX2 => match (bank, addr) {
                (0x00..=0x3F, 0x0000..=0x7FFF) => self.rom[(addr + bank * 0x8000) & self.rom_mask],
                (0x00..=0x3F, 0x8000..=0xFFFF) => {
                    self.rom[(addr - 0x8000 + bank * 0x8000) & self.rom_mask]
                }
                (0x40..=0x5F, _) => self.rom[((bank - 0x40) * 0x10000 + addr) & self.rom_mask],
                (0x70..=0x71, _) => self.ram[((bank - 0x70) * 0x10000 + addr) & self.ram_mask],
                _ => {
                    println!("GSU reading unmapped address: {:06X}", fulladdr);
                    0
                }
            },
        }
    }

    pub fn read16_bus(&self, fulladdr: GsuAddress) -> u16 {
        (self.read_bus(fulladdr) as u16) | ((self.read_bus(gsu_addr_add(fulladdr, 1)) as u16) << 8)
    }

    fn read_bus_tick(&mut self, fulladdr: GsuAddress) -> u8 {
        // TODO bus access clear check

        // Cycles counted as if GSU is running on 21MHz
        let cycles = match self.determine_bus(fulladdr) {
            GsuBus::ROM if self.regs.is_high_speed() => 5,
            GsuBus::ROM => 6, // actually 3
            GsuBus::RAM if self.regs.is_high_speed() => 5,
            GsuBus::RAM => 6, // actually 3
            GsuBus::Cache if self.regs.is_high_speed() => 1,
            GsuBus::Cache => 2, // actually 1
        };
        self.cycles_raw(cycles);

        self.read_bus(fulladdr)
    }

    pub fn read16_bus_tick(&mut self, fulladdr: GsuAddress) -> u16 {
        (self.read_bus_tick(fulladdr) as u16)
            | ((self.read_bus_tick(gsu_addr_add(fulladdr, 1)) as u16) << 8)
    }

    fn fetch(&mut self) -> u8 {
        let pc = self.regs.get_full_pc();

        if let Some(branch_pc) = self.regs.get_clr_r15_shadow() {
            // Branch scheduled, now in delay slot
            // Next fetch will be from the new position.
            self.regs.set_r15(branch_pc);
        } else {
            self.regs.set_r15(self.regs.get_r15().wrapping_add(1));
        }

        if let Some(b) = self.read_cache(pc) {
            b
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

    fn get_speed_factor(&self) -> Ticks {
        if self.regs.is_high_speed() {
            1
        } else {
            2
        }
    }

    pub fn step(&mut self) -> Result<Ticks> {
        // Check if code region is available
        match self.determine_bus(self.regs.get_full_pc()) {
            GsuBus::ROM if !self.regs.get_scmr_ron() => return Ok(1),
            GsuBus::RAM if !self.regs.get_scmr_ran() => return Ok(1),
            _ => (),
        }

        let start_cycles = self.cycles;
        let instr = self.fetch();

        // Note: ALTx is ignored if the opcode following does not
        // require ALTx.
        let alt1 = self.regs.test_flag(Flag::ALT1);
        let alt2 = self.regs.test_flag(Flag::ALT2);

        let sreg = self.sreg;
        let dreg = self.dreg;
        let flag_b = self.regs.test_flag(Flag::B);

        if self.verbose {
            println!("{}", self.regs);
            println!("SREG: R{} DREG: R{}", sreg, dreg);
            println!(
                "--> {}",
                Self::instr_str(instr, alt1, alt2, flag_b, sreg, dreg)
            );
        }

        // SREG/DREG/ALTx are reset after execution, but should persist
        // for branch and prefix instructions.
        // Special cases for MOVE/MOVES because they use the same opcode
        // as TO/FROM.
        if (!(0x05..=0x0F).contains(&instr)
            && !(0x3D..=0x3F).contains(&instr)
            && (instr & 0xF0) != 0x10
            && (instr & 0xF0) != 0x20
            && (instr & 0xF0) != 0xB0)
            || (self.regs.test_flag(Flag::B) && ((instr & 0xF0 == 0x10) || (instr & 0xF0 == 0xB0)))
        {
            self.sreg = 0;
            self.dreg = 0;
            self.regs
                .write_flags(&[(Flag::ALT1, false), (Flag::ALT2, false), (Flag::B, false)]);
        }

        if self.regs.r14_written {
            // Update ROM buffer
            // (note: purposely not checking ROMBR for changes to emulate the glitch)
            let addr = (GsuAddress::from(self.regs.read(Register::ROMBR)) << 16)
                | GsuAddress::from(self.regs.read(Register::R14));

            if self.regs.read(Register::ROMBR) > 0x5F {
                println!(
                    "GSU: ROMBR out of range ({:02X})!",
                    self.regs.read(Register::ROMBR)
                );
            }

            self.rom_buffer = self.read_bus_tick(addr);
            self.regs.r14_written = false;
        }

        match (instr, alt1, alt2) {
            (0x00, false, false) => {
                // STOP
                self.pixelcache_flush();
                self.regs
                    .write_flags(&[(Flag::G, false), (Flag::IRQ, true)]);

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
            (0x03, false, false) => {
                // LSR
                let s = self.regs.read_r(sreg);
                let result = s >> 1;
                self.regs.write_r(dreg, result);
                self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    // Upper bit can never be set after right shift
                    (Flag::S, false),
                    (Flag::C, s & 0x01 != 0),
                ]);
                self.cycles(1)?;
            }
            (0x04, false, false) => {
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

                if flag_b {
                    // MOVE
                    let val = self.regs.read_r(sreg);
                    self.regs.write_r(reg, val);
                } else {
                    // TO
                    self.dreg = reg;
                }

                self.cycles(1)?;
            }
            (0x20..=0x2F, _, _) => {
                // WITH
                let reg = (instr & 0x0F) as usize;
                self.sreg = reg;
                self.dreg = reg;
                self.regs.write_flags(&[(Flag::B, true)]);
                // cycles unknown, assumed 3/3/1
                self.cycles(1)?;
            }
            (0x30..=0x3B, false, false) => {
                // STW (Rn)
                let addr = self.regs.read_r((instr & 0x0F) as usize);
                let addr_l =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(addr & !1);
                let addr_h =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(addr | 1);
                let v = self.regs.read_r(sreg);

                if addr & 1 != 0 {
                    self.ram[addr_h] = v as u8;
                    self.ram[addr_l] = (v >> 8) as u8;
                } else {
                    self.ram[addr_l] = v as u8;
                    self.ram[addr_h] = (v >> 8) as u8;
                }
                self.last_ramaddr = addr;
                self.cycles(1)?;
            }
            (0x30..=0x3B, true, false) => {
                // STB (Rn)
                let addr = (usize::from(self.regs.read(Register::RAMBR)) << 16)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize));
                let v = self.regs.read_r(sreg);
                // Ignores high byte
                self.ram[addr] = v as u8;
                self.last_ramaddr = addr as u16;
                self.cycles(1)?;
            }
            (0x3C, false, false) => {
                // LOOP
                let i = self.regs.read(Register::R12);
                let new_i = i.wrapping_sub(1);
                self.regs.write(Register::R12, new_i);
                self.regs
                    .write_flags(&[(Flag::S, new_i & 0x8000 != 0), (Flag::Z, new_i == 0)]);
                if new_i != 0 {
                    let new_pc = self.regs.read(Register::R13);
                    self.regs.write(Register::R15, new_pc);
                }
                self.cycles(1)?;
            }
            (0x3D, _, _) => {
                // ALT1
                self.regs.write_flags(&[(Flag::ALT1, true)]);
                self.cycles(1)?;
            }
            (0x3E, _, _) => {
                // ALT2
                self.regs.write_flags(&[(Flag::ALT2, true)]);
                self.cycles(1)?;
            }
            (0x3F, _, _) => {
                // ALT3
                self.regs
                    .write_flags(&[(Flag::ALT1, true), (Flag::ALT2, true)]);
                self.cycles(1)?;
            }
            (0x40..=0x4B, false, false) => {
                // LDW (Rn)
                let addr = self.regs.read_r((instr & 0x0F) as usize);
                let addr_l =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(addr & !1);
                let addr_h =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(addr | 1);
                let v = if addr & 1 != 0 {
                    ((self.ram[addr_l] as u16) << 8) | (self.ram[addr_h] as u16)
                } else {
                    self.ram[addr_l] as u16 | ((self.ram[addr_h] as u16) << 8)
                };

                self.last_ramaddr = addr;
                self.regs.write_r(dreg, v);
                self.cycles(7)?;
            }
            (0x40..=0x4B, true, false) => {
                // LDB (Rn)
                let addr_l = (usize::from(self.regs.read(Register::RAMBR)) << 16)
                    | usize::from(self.regs.read_r((instr & 0x0F) as usize));
                // Zero-expanded
                let v = self.ram[addr_l] as u16;
                self.last_ramaddr = addr_l as u16;
                self.regs.write_r(dreg, v);
                self.cycles(6)?;
            }
            (0x4C, false, false) => {
                // PLOT
                self.pixel_draw();
                let _ = self.regs.read_inc(Register::R1);
                self.cycles(1)?;
            }
            (0x4C, true, false) => {
                // RPIX
                self.pixelcache_flush();

                let result = self.pixel_read();
                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x4D, false, false) => {
                // SWAP
                let s = self.regs.read_r(sreg);
                let result = s.rotate_right(8);

                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x4E, false, false) => {
                // COLOR
                self.set_color(self.regs.read_r(sreg) as u8);
                self.cycles(1)?;
            }
            (0x4E, true, false) => {
                // CMODE
                let s = self.regs.read_r(sreg) & 0x1F;
                self.regs.write(Register::POR, s);
                self.cycles(1)?;
            }
            (0x4F, false, false) => {
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
                let result =
                    (self.regs.read(Register::R7) & 0xFF00) | (self.regs.read(Register::R8) >> 8);
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
            (0x90, false, false) => {
                // SBK
                let addr = usize::from(self.regs.read(Register::RAMBR)) << 16
                    | usize::from(self.last_ramaddr);
                let v = self.regs.read_r(sreg);

                if addr & 1 != 0 {
                    self.ram[addr | 1] = v as u8;
                    self.ram[addr & !1] = (v >> 8) as u8;
                } else {
                    self.ram[addr & !1] = v as u8;
                    self.ram[addr | 1] = (v >> 8) as u8;
                }
                self.cycles(1)?;
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
            (0x95, false, false) => {
                // SEX
                let s = self.regs.read_r(sreg) & 0xFF;
                let result = s as i8 as i16 as u16;

                self.regs.write_r(dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0x96, false, false) => {
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
            (0x96, true, false) => {
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
            (0x97, false, false) => {
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
            (0x98..=0x9D, false, false) => {
                // JMP Rn
                let r = (instr & 0x0F) as usize;
                self.regs.write(Register::R15, self.regs.read_r(r));
                self.cycles(1)?;
            }
            (0x98..=0x9D, true, false) => {
                // LJMP Rn
                let r = (instr & 0x0F) as usize;
                self.regs.write(Register::R15, self.regs.read_r(sreg));
                self.regs.write(Register::PBR, self.regs.read_r(r) & 0x7F);
                self.regs
                    .write(Register::CBR, self.regs.read_r(sreg) & 0xFFF0);
                self.cache_flush();
                self.cycles(1)?;
            }
            (0x9E, false, false) => {
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
            (0xA0..=0xAF, false, false) => {
                // IBT Rn,imm
                let reg = (instr & 0x0F) as usize;
                let s = self.fetch() as u16;
                let result = s as i8 as i16 as u16;

                self.regs.write_r(reg, result);
                self.cycles(2)?;
            }
            (0xA0..=0xAF, true, false) => {
                // LMS Rn,(yy)
                let reg = (instr & 0x0F) as usize;
                let yy = (self.fetch() as u16) << 1;

                let addr_l = (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(yy);
                let addr_h = addr_l | 1;

                let val = ((self.ram[addr_h] as u16) << 8) | (self.ram[addr_l] as u16);
                self.last_ramaddr = yy;
                self.regs.write_r(reg, val);
                self.cycles(7)?;
            }
            (0xA0..=0xAF, false, true) => {
                // SMS (yy),Rn
                let reg = (instr & 0x0F) as usize;
                let yy = (self.fetch() as u16) << 1;

                let addr_l = (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(yy);
                let addr_h = addr_l | 1;
                let val = self.regs.read_r(reg);
                self.ram[addr_l] = val as u8;
                self.ram[addr_h] = (val >> 8) as u8;
                self.last_ramaddr = yy;
                self.cycles(1)?;
            }
            (0xB0..=0xBF, _, _) => {
                // FROM/MOVES
                let reg = (instr & 0x0F) as usize;

                if flag_b {
                    // MOVES
                    let val = self.regs.read_r(reg);
                    self.regs.write_r(dreg, val);
                    self.regs.write_flags(&[
                        (Flag::S, val & 0x8000 != 0),
                        (Flag::Z, val == 0),
                        (Flag::V, val & 0x80 != 0),
                    ]);
                } else {
                    // FROM
                    self.sreg = reg;
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
            (0xD0..=0xDE, _, false) => {
                // INC Rn
                let reg = (instr & 0x0F) as usize;
                let result = self.regs.read_r(reg).wrapping_add(1);
                self.regs.write_r(reg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(1)?;
            }
            (0xDF, false, false) => {
                // GETC
                self.set_color(self.rom_buffer);
                self.cycles(1)?;
            }
            (0xDF, false, true) => {
                // RAMB
                self.regs
                    .write(Register::RAMBR, self.regs.read_r(sreg) & 0x01);
                self.cycles(1)?;
            }
            (0xDF, true, true) => {
                // ROMB
                self.regs
                    .write(Register::ROMBR, self.regs.read_r(sreg) & 0x7F);
                self.cycles(1)?;
            }
            (0xE0..=0xEE, false, _) => {
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
                let val = self.rom_buffer as u16;
                let s = self.regs.read_r(sreg);
                self.regs.write_r(
                    dreg,
                    match (alt1, alt2) {
                        // GETB
                        (false, false) => val,
                        // GETBH
                        (true, false) => (val << 8) | (s & 0x00FF),
                        // GETBL
                        (false, true) => val | (s & 0xFF00),
                        // GETBS
                        (true, true) => self.rom_buffer as i8 as i16 as u16,
                    },
                );
                self.cycles(1)?;
            }
            (0xF0..=0xFF, false, false) => {
                // IWT
                let reg = (instr & 0x0F) as usize;
                let imm = self.fetch16();
                self.regs.write_r(reg, imm);
                self.cycles(3)?;
            }
            (0xF0..=0xFF, false, true) => {
                // SM (hilo),Rn
                let reg = (instr & 0x0F) as usize;
                let lo = self.fetch() as u16;
                let hi = self.fetch() as u16;
                let yy = (hi << 8) | lo;

                let addr_l =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(yy & !1);
                let addr_h =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(yy | 1);
                let val = self.regs.read_r(reg);
                if yy & 1 != 0 {
                    self.ram[addr_h] = val as u8;
                    self.ram[addr_l] = (val >> 8) as u8;
                } else {
                    self.ram[addr_l] = val as u8;
                    self.ram[addr_h] = (val >> 8) as u8;
                }
                self.last_ramaddr = yy;
                self.cycles(4)?;
            }
            (0xF0..=0xFF, true, false) => {
                // LM Rn,(hilo)
                let reg = (instr & 0x0F) as usize;
                let lo = self.fetch() as u16;
                let hi = self.fetch() as u16;
                let yy = (hi << 8) | lo;

                let addr_l =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(yy & !1);
                let addr_h =
                    (usize::from(self.regs.read(Register::RAMBR)) << 16) | usize::from(yy | 1);
                let val = if yy & 1 != 0 {
                    ((self.ram[addr_l] as u16) << 8) | (self.ram[addr_h] as u16)
                } else {
                    ((self.ram[addr_h] as u16) << 8) | (self.ram[addr_l] as u16)
                };
                self.last_ramaddr = yy;
                self.regs.write_r(reg, val);
                self.cycles(4)?;
            }
            _ => panic!(
                "Unimplemented instruction {:02X} alt1 = {} alt2 = {}",
                instr, alt1, alt2
            ),
        }

        Ok(self.cycles - start_cycles)
    }

    #[inline(always)]
    fn cycles(&mut self, cycles: Ticks) -> Result<()> {
        self.cycles += cycles * self.get_speed_factor();
        Ok(())
    }

    #[inline(always)]
    fn cycles_raw(&mut self, cycles: Ticks) {
        self.cycles += cycles;
    }

    fn pixelcache_flush(&mut self) {
        if self.pixelcache_drawn != 0 {
            let bpp = self.regs.get_scmr_bpp();

            // Count cycles for read AND write
            if self.regs.is_high_speed() {
                self.cycles_raw(2 * 5 * bpp.num_bitplanes());
            } else {
                self.cycles_raw(2 * 6 * bpp.num_bitplanes());
            }
        }

        self.pixelcache_drawn = 0;
    }

    fn pixel_draw(&mut self) {
        let x = (self.regs.read(Register::R1) as usize) & 0xFF;
        let y = (self.regs.read(Register::R2) as usize) & 0xFF;
        let ocolor = self.regs.read(Register::COLR) as u8;
        let color = if self.regs.test_por(PORFlag::Dither)
            && ((x ^ y) & 1) == 1
            && self.regs.get_scmr_bpp() != BPP::Eight
        {
            ocolor / 0x10
        } else {
            ocolor
        };

        if (x & 0xF8) != self.pixelcache_x || y != self.pixelcache_y {
            self.pixelcache_flush();
            self.pixelcache_x = x & 0xF8;
            self.pixelcache_y = y;
        }

        // Transparency
        let bpp = self.regs.get_scmr_bpp();
        if !self.regs.test_por(PORFlag::NotTransparent) {
            let trans_mask = if self.regs.test_por(PORFlag::ColorHighFreeze) {
                bpp.color_mask() & 0x0F
            } else {
                bpp.color_mask()
            };
            if (color & trans_mask) == 0 {
                return;
            }
        }

        self.pixelcache_drawn |= 1 << (x & 0x07);

        let row_addr = self.xy_to_row_addr(x, y);
        let bit = 1 << (7 - (x & 7));

        for bitp in 0..(bpp.num_bitplanes()) {
            let addr = row_addr + Self::BITPLANE_OFFSETS[bitp];

            if color & (1 << bitp) != 0 {
                self.ram[addr] |= bit;
            } else {
                self.ram[addr] &= !bit;
            }
        }

        if self.pixelcache_drawn == 0xFF {
            self.pixelcache_flush();
        }
    }

    fn pixel_read(&mut self) -> u16 {
        let x = (self.regs.read(Register::R1) as usize) & 0xFF;
        let y = (self.regs.read(Register::R2) as usize) & 0xFF;
        let bpp = self.regs.get_scmr_bpp();

        let row_addr = self.xy_to_row_addr(x, y);
        let bit = 1 << (7 - (x & 7));

        let mut ret = 0;
        for bitp in 0..(bpp.num_bitplanes()) {
            let addr = row_addr + Self::BITPLANE_OFFSETS[bitp];

            if self.ram[addr] & bit != 0 {
                ret |= 1 << bitp;
            }
        }
        ret
    }

    fn xy_to_row_addr(&self, x: usize, y: usize) -> usize {
        let bpp = self.regs.get_scmr_bpp();

        let tilenum = if self.regs.get_scmr_height() == ScreenHeight::Obj
            || self.regs.test_por(PORFlag::ObjMode)
        {
            (y / 0x80) * 0x200 + (x / 0x80) * 0x100 + ((y / 8) & 0x0F) * 0x10 + ((x / 8) & 0x0F)
        } else {
            match self.regs.get_scmr_height() {
                ScreenHeight::H128 => (x / 8) * 0x10 + (y / 8),
                ScreenHeight::H160 => (x / 8) * 0x14 + (y / 8),
                ScreenHeight::H192 => (x / 8) * 0x18 + (y / 8),
                ScreenHeight::Obj => unreachable!(),
            }
        };
        let scbr = self.regs.read(Register::SCBR) as usize;
        match bpp {
            BPP::Two => tilenum * 0x10 + scbr * 0x400 + (y & 7) * 2,
            BPP::Four => tilenum * 0x20 + scbr * 0x400 + (y & 7) * 2,
            BPP::Eight => tilenum * 0x40 + scbr * 0x400 + (y & 7) * 2,
        }
    }

    fn op_branch(&mut self, cond: bool) {
        let dest = self.fetch() as i8 as i16;
        let pc = self.regs.read(Register::R15);
        let new_pc = pc.wrapping_add_signed(dest);

        if cond {
            self.regs.write(Register::R15, new_pc);
        }
        // TODO cycles
    }

    pub fn get_int(&mut self) -> bool {
        self.regs.test_flag(Flag::IRQ) && !self.regs.test_cfgr(CFGRFlag::IRQ)
    }

    pub fn instr_str(
        instr: u8,
        alt1: bool,
        alt2: bool,
        b_flag: bool,
        sreg: usize,
        dreg: usize,
    ) -> String {
        let l = instr & 0x0F;
        match (instr, alt1, alt2) {
            (0x00, false, false) => "STOP".to_string(),
            (0x01, false, false) => "NOP".to_string(),
            (0x03, false, false) => format!("LSR R{} = R{} << 1", dreg, sreg),
            (0x04, false, false) => "ROL".to_string(),
            (0x09, _, _) => "BEQ".to_string(),
            (0x10..=0x1F, _, _) if b_flag => format!("MOVE R{}, R{}", l, sreg),
            (0x10..=0x1F, _, _) => format!("TO R{}", l),
            (0x20..=0x2F, false, false) => format!("WITH R{}", l),
            (0x3C, false, false) => "LOOP".to_string(),
            (0x97, false, false) => "ROR".to_string(),
            (0xB0..=0xBF, _, _) if b_flag => format!("MOVES R{}, R{}", dreg, sreg),
            (0xB0..=0xBF, _, _) => format!("FROM R{}", l),
            (0xC1..=0xCF, false, false) => format!("OR R{}", l),
            (0xC1..=0xCF, false, true) => format!("OR #{:02X}", l),
            _ => format!(
                "? {} {:02X}",
                match (alt1, alt2) {
                    (false, false) => "",
                    (true, false) => "ALT1",
                    (false, true) => "ALT2",
                    (true, true) => "ALT3",
                },
                instr
            ),
        }
    }

    fn set_color(&mut self, v: u8) {
        let cur = self.regs.read8(Register::COLR);
        self.regs.write8(
            Register::COLR,
            if self.regs.test_por(PORFlag::ColorHighNibble) {
                (cur & 0xF0) | (v >> 4)
            } else if self.regs.test_por(PORFlag::ColorHighFreeze) {
                (cur & 0xF0) | (v & 0x0F)
            } else {
                v
            },
        );
    }
}

impl Tickable for CpuGsu {
    fn tick(&mut self, _ticks: Ticks) -> Result<Ticks> {
        if !self.regs.test_flag(Flag::G) {
            // GSU stopped
            return Ok(0);
        }

        self.step()
    }
}
