use anyhow::Result;
use serbia::serbia;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, Bus, ADDRESS_MASK};
use crate::tickable::{Tickable, Ticks};

/// Peripherals as they face the SA-1 65816
#[serbia]
#[derive(Serialize, Deserialize)]
pub struct Sa1Bus {}

impl Sa1Bus {
    pub fn new() -> Self {
        Self {}
    }
}

impl Bus<Address> for Sa1Bus {
    fn read(&self, addr: Address) -> u8 {
        0xFF
    }

    fn write(&mut self, addr: Address, val: u8) {}

    fn get_mask(&self) -> Address {
        ADDRESS_MASK
    }

    fn get_clr_nmi(&mut self) -> bool {
        false
    }

    fn get_int(&mut self) -> bool {
        false
    }
}

impl Tickable for Sa1Bus {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        Ok(ticks)
    }
}
