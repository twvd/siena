use std::cell::RefCell;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, BusMember};
use crate::cpu_gsu::cpu::CpuGsu;
use crate::tickable::{Tickable, Ticks};

/// SuperFX co-processor
#[derive(Serialize, Deserialize)]
pub struct SuperFX {
    /// GSU CPU core
    cpu: RefCell<CpuGsu>,
}

impl SuperFX {
    pub fn new() -> Self {
        Self {
            cpu: RefCell::new(CpuGsu::new()),
        }
    }
}

impl Tickable for SuperFX {
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        let mut cpu = self.cpu.borrow_mut();

        Ok(())
    }
}

impl BusMember<Address> for SuperFX {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        println!("SuperFX read: {:04X}", addr);

        None
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        println!("SuperFX write: {:04X} {:02X}", addr, val);
        None
    }
}
