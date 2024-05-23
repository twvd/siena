use std::cell::Cell;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, BusMember};
use crate::tickable::{Tickable, Ticks};

/// Suoer Gameboy co-processor
#[derive(Serialize, Deserialize)]
pub struct SuperGameboy {
    pub rownr: Cell<u16>,
    pub buffernr: Cell<u8>,
}

impl SuperGameboy {
    pub fn new() -> Self {
        Self {
            rownr: Cell::new(0x11 << 4),
            buffernr: Cell::new(0),
        }
    }
}

impl Tickable for SuperGameboy {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        Ok(1)
    }
}

impl BusMember<Address> for SuperGameboy {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x6000 => {
                self.rownr.set((self.rownr.get() + 1) % 0x120);
                if self.rownr.get() == 0 {
                    self.buffernr.set((self.buffernr.get() + 1) % 4);
                }
                println!("read row: {:X}", self.rownr.get() >> 4);
                Some((((self.rownr.get() >> 4) as u8) << 3) | (self.buffernr.get()))
            }
            // Packet available flag
            0x6002 => Some(0),
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match addr {
            0x6003 => {
                if val & 0x80 != 0 {
                    self.rownr.set(0);
                }
                Some(())
            }
            _ => None,
        }
    }
}
