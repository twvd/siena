use anyhow::Result;

use crate::snes::bus::{Bus, BusMember};
use crate::snes::cpu_spc700::cpu::{SpcAddress, SPC_ADDRESS_MASK};
use crate::tickable::{Tickable, Ticks};

const APU_RAM_SIZE: usize = 64 * 1024;
const APU_ROM_SIZE: usize = 64;

/// APU peripherals as they face the SPC700 audio CPU
pub struct Apubus {
    ram: [u8; APU_RAM_SIZE],
    rom: [u8; APU_ROM_SIZE],
}

impl Apubus {
    pub fn new(rom: &[u8]) -> Self {
        Self {
            ram: [0; APU_RAM_SIZE],
            rom: rom.try_into().unwrap(),
        }
    }
}

impl Bus<SpcAddress> for Apubus {
    fn read(&self, addr: SpcAddress) -> u8 {
        match addr {
            // ROM (IPL)
            // TODO mask setting!
            0xFFC0..=0xFFFF => self.rom[addr as usize - 0xFFC0],
            _ => self.ram[addr as usize],
        }
    }

    fn write(&mut self, addr: SpcAddress, val: u8) {
        // Writes ALWAYS go through to RAM
        self.ram[addr as usize] = val;
    }

    fn get_mask(&self) -> SpcAddress {
        SPC_ADDRESS_MASK
    }

    fn get_clr_nmi(&mut self) -> bool {
        // TODO refactor this trait to remove this
        unreachable!()
    }

    fn get_clr_int(&mut self) -> bool {
        // TODO refactor this trait to remove this
        unreachable!()
    }
}

impl Tickable for Apubus {
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        // This ticks at the speed of the APU CPU,
        // not the master clock!
        Ok(())
    }
}
