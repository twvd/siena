use std::cell::RefCell;

use anyhow::Result;
use serde::{Deserialize, Serialize};
use snes_spc::{SnesSpc, SpcTime};

use crate::bus::{Address, BusMember};
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
    const SAMPLE_BUFFER_SIZE: usize = 256;

    const IPL_SIZE: usize = 64;

    pub fn new(ipl: &[u8], _verbose: bool) -> Self {
        assert_eq!(ipl.len(), Self::IPL_SIZE);

        let mut spc = SnesSpc::from_ipl(Self::SAMPLE_BUFFER_SIZE, ipl).unwrap();
        spc.reset();
        spc.reset_output();
        Self {
            spc: Some(RefCell::new(spc)),
            ticks: 0,
        }
    }

    pub fn render(&mut self, out: &mut [i16]) {
        if let Some(spc) = &self.spc {
            spc.borrow_mut().play(out).unwrap();
        }
    }
}

impl Tickable for Apu {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        self.ticks += ticks;

        if self.ticks >= 100 {
            if let Some(spc) = &self.spc {
                spc.borrow_mut().end_frame(self.ticks as SpcTime);
            }
            self.ticks = 0;
        }

        Ok(ticks)
    }
}

impl BusMember<Address> for Apu {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x2140..=0x217F => {
                let ch = (addr - 0x2140) % 4;

                if let Some(spc) = &self.spc {
                    Some(spc.borrow_mut().read_port(self.ticks as SpcTime, ch as u8) as u8)
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
                    spc.borrow_mut()
                        .write_port(self.ticks as SpcTime, ch as u8, val);
                }
                Some(())
            }
            _ => None,
        }
    }
}
