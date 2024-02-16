use serde::{Deserialize, Serialize};

use super::regs::RegisterFile;

use crate::tickable::Ticks;

pub type GsuAddress = u32;
pub const GSU_ADDRESS_MASK: GsuAddress = 0xFFFFFF;

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

        // TODO cache
        match (bank & !0x80, addr) {
            (0x00..=0x3F, 0x8000..=0xFFFF) => self.rom[addr - 0x8000 + bank * 0x8000],
            (0x40..=0x5F, _) => self.rom[(bank - 0x40) * 0x10000 + addr],
            (0x70..=0x71, _) => self.ram[(bank - 0x70) * 0x10000 + addr],
            _ => panic!("Unmapped address"),
        }
    }
}
