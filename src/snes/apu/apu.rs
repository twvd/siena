use std::sync::{Arc, RwLock};

use anyhow::Result;
use colored::*;
use serde::{Deserialize, Serialize};

use crate::snes::bus::{Address, BusMember};
use crate::snes::cpu_spc700::cpu::{CpuSpc700, SpcAddress};
use crate::snes::cpu_spc700::regs::Register;
use crate::tickable::{Tickable, Ticks};

use super::apubus::Apubus;

/// Type for the CPU <-> APU communication ports
pub type ApuPorts = Arc<RwLock<InnerApuPorts>>;

/// Type for the CPU <-> APU communication ports
#[derive(Serialize, Deserialize)]
pub struct InnerApuPorts {
    /// APU -> CPU
    pub cpu: [u8; 4],

    /// CPU -> APU
    pub apu: [u8; 4],

    /// Trace S-CPU <-> S-APU communication
    pub trace: bool,
}

/// The SNES Audio Processing Unit
#[derive(Serialize, Deserialize)]
pub struct Apu {
    /// SPC700 CPU core
    pub cpu: CpuSpc700<Apubus>,

    /// SPC cycles taken last step that we need to wait out.
    spc_cycles_taken: Ticks,

    /// Master ticks received that we can spend.
    spc_master_credit: Ticks,

    /// Print instructions
    pub verbose: bool,

    /// Main CPU communication ports
    pub ports: ApuPorts,
}

impl Apu {
    /// One SPC cycle = 3 master cycles
    const SPC_MASTER_FACTOR: Ticks = 3;

    const IPL_ENTRYPOINT: SpcAddress = 0xFFC0;
    const IPL_SIZE: usize = 64;

    pub fn new(ipl: &[u8], verbose: bool) -> Self {
        assert_eq!(ipl.len(), Self::IPL_SIZE);

        let ports = Arc::new(RwLock::new(InnerApuPorts {
            cpu: [0; 4],
            apu: [0; 4],
            trace: false,
        }));
        Self {
            cpu: CpuSpc700::<Apubus>::new(
                Apubus::new(ipl, Arc::clone(&ports)),
                Self::IPL_ENTRYPOINT,
            ),
            spc_cycles_taken: 0,
            spc_master_credit: 0,
            verbose,
            ports,
        }
    }

    /// Get a (reference counted) copy of the communication ports
    pub fn get_ports(&self) -> ApuPorts {
        Arc::clone(&self.ports)
    }

    pub fn render(&mut self, out: &mut [i16]) {
        // Stub until DSP is implemented
        for i in 0..out.len() {
            out[i] = 0;
        }
    }
}

impl Tickable for Apu {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // Step the SPC700 every 3 CPU clock ticks
        // and wait for every SPC cycle consumed so they
        // run somewhat in sync.
        self.spc_master_credit += ticks;

        while self.spc_master_credit >= Self::SPC_MASTER_FACTOR {
            if self.spc_cycles_taken == 0 {
                if self.verbose && self.cpu.regs.read(Register::PC) < Self::IPL_ENTRYPOINT {
                    println!("{}", self.cpu.dump_state().red());
                }
                self.spc_cycles_taken += self.cpu.step()?;
            }
            self.spc_cycles_taken -= 1;
            self.spc_master_credit -= Self::SPC_MASTER_FACTOR;
        }

        Ok(())
    }
}

impl BusMember<Address> for Apu {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x2140..=0x217F => {
                let ch = (addr - 0x2140) % 4;

                let ports = self.ports.read().unwrap();
                Some(ports.cpu[ch])
            }
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x2140..=0x217F => {
                let ch = (addr - 0x2140) % 4;
                let mut ports = self.ports.write().unwrap();
                if ports.trace {
                    println!(
                        "{} ({:04X}) to {} ({}): {:02X}",
                        "CPU".green(),
                        addr,
                        "APU".red(),
                        ch,
                        val
                    );
                }
                ports.apu[ch] = val;
                Some(())
            }
            _ => None,
        }
    }
}
