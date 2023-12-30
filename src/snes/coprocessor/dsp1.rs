use serde::{Deserialize, Serialize};

use crate::snes::bus::{Address, BusMember};

/// DSP-1 co-processor
#[derive(Serialize, Deserialize)]
pub struct DSP1 {}

impl DSP1 {
    pub fn new() -> Self {
        Self {}
    }
}

impl BusMember<Address> for DSP1 {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x7000 => {
                // SR (LSB)
                Some(0)
            }
            0x7001 => {
                // SR (MSB)
                Some(0x80)
            }
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            _ => None,
        }
    }
}
