use anyhow::Result;

use crate::snes::bus::{Address, Bus, BusMember};
use crate::tickable::{Tickable, Ticks};

/// All perioherals as they face the main CPU
pub struct Mainbus {
    cartridge: Vec<u8>,
}

impl Mainbus {
    pub fn new(cartridge: &[u8]) -> Self {
        Self {
            cartridge: cartridge.to_owned(),
        }
    }
}

impl Bus for Mainbus {}

impl BusMember for Mainbus {
    fn read(&self, fulladdr: Address) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match bank {
            0x00..=0x3F => match addr {
                // WS1 LoROM
                0x8000..=0xFFFF => self.cartridge[addr - 0x8000 + bank * 0x8000],

                _ => 0xFF,
            },
            _ => 0xFF,
        }
    }

    fn write(&mut self, addr: Address, val: u8) {}
}

impl Tickable for Mainbus {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        Ok(())
    }
}
