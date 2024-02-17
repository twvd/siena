use anyhow::Result;
use serde::{Deserialize, Serialize};

use super::regs::{Flag, Register, RegisterFile};

use crate::tickable::{Tickable, Ticks};

pub type GsuAddress = u32;
pub const GSU_ADDRESS_MASK: GsuAddress = 0xFFFFFF;
fn gsu_addr_add(addr: GsuAddress, i: GsuAddress) -> GsuAddress {
    (addr & 0xFF0000) | (addr.wrapping_add(i) & 0xFFFF)
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
    pub regs: RegisterFile,
    pub cycles: Ticks,
    pub cache: Vec<u8>,
    pub rom: Vec<u8>,
    pub ram: Vec<u8>,

    sreg: usize,
    dreg: usize,
}

impl CpuGsu {
    pub fn new(rom: &[u8]) -> Self {
        let mut c = Self {
            regs: RegisterFile::new(),
            cycles: 0,
            cache: vec![0; 512],
            rom: vec![0xFF; 8 * 1024 * 1024],
            ram: vec![0xFF; 256 * 1024],
            sreg: 0,
            dreg: 0,
        };

        c.rom[0..rom.len()].copy_from_slice(rom);
        c
    }

    pub fn determine_bus(&self, fulladdr: GsuAddress) -> GsuBus {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        // TODO cache
        match (bank & !0x80, addr) {
            (0x00..=0x3F, 0x8000..=0xFFFF) => GsuBus::ROM,
            (0x40..=0x5F, _) => GsuBus::ROM,
            (0x70..=0x71, _) => GsuBus::RAM,
            _ => panic!("Unmapped address"),
        }
    }

    pub fn read_bus(&self, fulladdr: GsuAddress) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        // TODO bus access clear check

        // TODO cache
        match (bank & !0x80, addr) {
            (0x00..=0x3F, 0x8000..=0xFFFF) => self.rom[addr - 0x8000 + bank * 0x8000],
            (0x40..=0x5F, _) => self.rom[(bank - 0x40) * 0x10000 + addr],
            (0x70..=0x71, _) => self.ram[(bank - 0x70) * 0x10000 + addr],
            _ => panic!("Unmapped address"),
        }
    }

    pub fn read16_bus(&self, fulladdr: GsuAddress) -> u16 {
        (self.read_bus(fulladdr) as u16) | ((self.read_bus(gsu_addr_add(fulladdr, 1)) as u16) << 8)
    }

    fn fetch(&mut self) -> u8 {
        let pc_bank = GsuAddress::from(self.regs.read(Register::PBR)) << 16;
        let pc = pc_bank | GsuAddress::from(self.regs.read_inc(Register::R15));
        self.read_bus(pc)
    }

    fn fetch16(&mut self) -> u16 {
        let lo = self.fetch() as u16;
        let hi = self.fetch() as u16;
        lo | (hi << 8)
    }

    pub fn step(&mut self) -> Result<()> {
        let instr = self.fetch();

        match instr {
            0x00 => {
                // STOP
                self.regs.write_flags(&[(Flag::G, false)]);
                self.cycles(3, 3, 1)?;
            }
            0x01 => {
                // NOP
                self.cycles(3, 3, 1)?;
            }
            0x10..=0x1F => {
                // TO
                let reg = (instr & 0x0F) as usize;
                self.dreg = reg;
                self.cycles(3, 3, 1)?;
            }
            0x20..=0x2F => {
                // WITH
                let reg = (instr & 0x0F) as usize;
                self.sreg = reg;
                self.dreg = reg;
                // cycles unknown, assumed 3/3/1
                self.cycles(3, 3, 1)?;
            }
            0x3D => {
                // ALT1
                self.regs.write_flags(&[(Flag::ALT1, true)]);
                self.cycles(3, 3, 1)?;
            }
            0x3E => {
                // ALT2
                self.regs.write_flags(&[(Flag::ALT2, true)]);
                self.cycles(3, 3, 1)?;
            }
            0x3F => {
                // ALT3
                self.regs
                    .write_flags(&[(Flag::ALT1, true), (Flag::ALT2, true)]);
                self.cycles(3, 3, 1)?;
            }
            0x4F => {
                // NOT
                let result = !self.regs.read_r(self.sreg);
                self.regs.write_r(self.dreg, result);
                self.regs
                    .write_flags(&[(Flag::Z, result == 0), (Flag::S, result & 0x8000 != 0)]);
                self.cycles(3, 3, 1)?;
            }
            0xB0..=0xBF => {
                // FROM
                let reg = (instr & 0x0F) as usize;
                self.sreg = reg;
                self.cycles(3, 3, 1)?;
            }
            0xF0..=0xFF => {
                // IWT
                let reg = (instr & 0x0F) as usize;
                let imm = self.fetch16();
                self.regs.write_r(reg, imm);
                self.cycles(9, 9, 3)?;
            }
            _ => panic!("Unimplemented instruction {:02X}", instr),
        }
        Ok(())
    }

    fn cycles(&mut self, _cy_rom: Ticks, _cy_ram: usize, _cy_cache: usize) -> Result<()> {
        // TODO cycles

        Ok(())
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
