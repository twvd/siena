use std::cell::RefCell;

use anyhow::Result;
use serde::{Serialize, Deserialize};
use snes_spc::{SnesSpc, SpcTime};

use crate::snes::bus::{Address, BusMember};
use crate::tickable::{Tickable, Ticks};

/// The SNES Audio Processing Unit
/// (interface layer for Blargg's S-APU)
#[derive(Serialize, Deserialize)]
pub struct Apu {
    #[serde(skip)]
    spc: Option<RefCell<SnesSpc>>,

    ticks: Ticks,
}

impl Apu {
    /// One SPC cycle = 3 master cycles
    const SPC_MASTER_FACTOR: Ticks = 3;

    const IPL_SIZE: usize = 64;

    const IPL_BIN: [u8; Self::IPL_SIZE] = [
        0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0, 0xFC, 0x8F, 0xAA, 0xF4, 0x8F, 0xBB, 0xF5,
        0x78, 0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4, 0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B,
        0xE4, 0xF5, 0xCB, 0xF4, 0xD7, 0x00, 0xFC, 0xD0, 0xF3, 0xAB, 0x01, 0x10, 0xEF, 0x7E, 0xF4,
        0x10, 0xEB, 0xBA, 0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD, 0x5D, 0xD0, 0xDB, 0x1F,
        0x00, 0x00, 0xC0, 0xFF,
    ];

    pub fn new(_verbose: bool) -> Self {
        Self {
            spc: Some(RefCell::new(SnesSpc::from_ipl(&Self::IPL_BIN).unwrap())),
            ticks: 0
        }
    }
}

impl Tickable for Apu {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        self.ticks += ticks;
        Ok(())
    }
}

impl BusMember<Address> for Apu {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x2140..=0x217F => {
                let ch = (addr - 0x2140) % 4;

                if let Some(spc) = &self.spc {
                    Some(spc.borrow_mut().read_port((self.ticks / Self::SPC_MASTER_FACTOR) as SpcTime, ch as u8) as u8)
                } else {
                    Some(0)
                }
            }
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x2140..=0x217F => {
                let ch = (addr - 0x2140) % 4;
                if let Some(spc) = &self.spc {
                    spc.borrow_mut().write_port((self.ticks / Self::SPC_MASTER_FACTOR) as SpcTime, ch as u8, val);
                }
                Some(())
            }
            _ => None,
        }
    }
}
