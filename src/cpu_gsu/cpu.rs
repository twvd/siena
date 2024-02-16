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
}

impl CpuGsu {
    pub fn new(rom: &[u8]) -> Self {
        let mut c = Self {
            regs: RegisterFile::new(),
            cycles: 0,
            cache: vec![0; 512],
            rom: vec![0xFF; 8 * 1024 * 1024],
            ram: vec![0xFF; 256 * 1024],
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

    pub fn step(&mut self) -> Result<()> {
        let pc_bank = GsuAddress::from(self.regs.read(Register::PBR)) << 16;
        let pc_addr = GsuAddress::from(self.regs.read(Register::R15));
        let pc_plus = |i| gsu_addr_add(pc_bank | pc_addr, i);

        let instr = self.read_bus(pc_bank | pc_addr);

        match instr {
            0x00 => {
                // STOP
                self.regs.write_flags(&[(Flag::G, false)]);
                self.finish_instr(1, 3, 3, 1)?;
            }
            0x01 => {
                // NOP
                self.finish_instr(1, 3, 3, 1)?;
            }
            0xF0..=0xFF => {
                // IWT
                let reg = (instr & 0x0F) as usize;
                let imm = self.read16_bus(pc_plus(1));
                self.regs.write_r(reg, imm);
                self.finish_instr(3, 9, 9, 3)?;
            }
            _ => panic!("Unimplemented instruction {:02X}", instr),
        }
        Ok(())
    }

    fn finish_instr(
        &mut self,
        len: usize,
        _cy_rom: Ticks,
        _cy_ram: usize,
        _cy_cache: usize,
    ) -> Result<()> {
        let pc = self.regs.read(Register::R15);
        self.regs.write(Register::R15, pc.wrapping_add(len as u16));

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
