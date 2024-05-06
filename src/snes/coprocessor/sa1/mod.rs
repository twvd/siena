mod bus;

use std::cell::RefCell;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, BusMember};
use crate::cpu_65816::cpu::Cpu65816;
use crate::tickable::{Tickable, Ticks};

use bus::Sa1Bus;

/// SA-1 co-processor
#[derive(Serialize, Deserialize)]
pub struct SA1 {
    /// CPU core
    pub cpu: RefCell<Cpu65816<Sa1Bus>>,
}

impl SA1 {
    pub fn new(rom: &[u8]) -> Self {
        Self {
            cpu: RefCell::new(Cpu65816::new(Sa1Bus::new(), 0)),
        }
    }
}

impl Tickable for SA1 {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        let mut cpu = self.cpu.borrow_mut();

        //cpu.tick(ticks)
        Ok(ticks)
    }
}

impl BusMember<Address> for SA1 {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        let mut cpu = self.cpu.borrow_mut();

        match addr {
            _ => None,
        }
    }
}
