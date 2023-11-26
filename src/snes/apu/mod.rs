pub mod apubus;

use anyhow::Result;

use crate::snes::cpu_spc700::cpu::{CpuSpc700, SpcAddress};
use crate::tickable::{Tickable, Ticks};

use apubus::Apubus;

/// The SNES Audio Processing Unit
pub struct Apu {
    /// SPC700 CPU core
    cpu: CpuSpc700<Apubus>,

    /// SPC cycles taken last step that we need to wait out.
    spc_cycles_taken: Ticks,

    /// Master ticks received that we can spend.
    spc_master_credit: Ticks,
}

impl Apu {
    /// One SPC cycle = 25 master cycles
    const SPC_MASTER_FACTOR: Ticks = 25;

    const IPL_ENTRYPOINT: SpcAddress = 0xFFC0;
    const IPL_SIZE: usize = 64;

    const IPL_BIN: [u8; Self::IPL_SIZE] = [
        0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0, 0xFC, 0x8F, 0xAA, 0xF4, 0x8F, 0xBB, 0xF5,
        0x78, 0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4, 0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B,
        0xE4, 0xF5, 0xCB, 0xF4, 0xD7, 0x00, 0xFC, 0xD0, 0xF3, 0xAB, 0x01, 0x10, 0xEF, 0x7E, 0xF4,
        0x10, 0xEB, 0xBA, 0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD, 0x5D, 0xD0, 0xDB, 0x1F,
        0x00, 0x00, 0xC0, 0xFF,
    ];

    pub fn new() -> Self {
        Self {
            cpu: CpuSpc700::<Apubus>::new(Apubus::new(&Self::IPL_BIN), Self::IPL_ENTRYPOINT),
            spc_cycles_taken: 0,
            spc_master_credit: 0,
        }
    }
}

impl Tickable for Apu {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // Step the SPC700 every 25 master clock ticks
        // and wait for every SPC cycle consumed so they
        // run somewhat in sync.
        self.spc_master_credit += ticks;

        while self.spc_master_credit >= Self::SPC_MASTER_FACTOR {
            if self.spc_cycles_taken == 0 {
                self.spc_cycles_taken += self.cpu.step()?;
            }
            self.spc_cycles_taken -= 1;
            self.spc_master_credit -= Self::SPC_MASTER_FACTOR;
        }

        Ok(())
    }
}
