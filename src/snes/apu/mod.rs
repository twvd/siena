pub mod apubus;
pub mod timers;

use std::cell::RefCell;
use std::rc::Rc;

use anyhow::Result;
use colored::*;
use serde::{Deserialize, Serialize};

use crate::snes::bus::{Address, BusMember};
use crate::snes::cpu_spc700::cpu::{CpuSpc700, SpcAddress};
use crate::snes::cpu_spc700::regs::Register;
use crate::tickable::{Tickable, Ticks};

use apubus::Apubus;

/// Type for the CPU <-> APU communication ports
#[derive(Serialize, Deserialize)]
pub struct ApuPorts {
    /// APU -> CPU
    cpu: [u8; 4],

    /// CPU -> APU
    apu: [u8; 4],

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
    pub ports: Rc<RefCell<ApuPorts>>,
}

impl Apu {
    /// One SPC cycle = 3 master cycles
    const SPC_MASTER_FACTOR: Ticks = 3;

    const IPL_ENTRYPOINT: SpcAddress = 0xFFC0;
    const IPL_SIZE: usize = 64;

    const IPL_BIN: [u8; Self::IPL_SIZE] = [
        0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0, 0xFC, 0x8F, 0xAA, 0xF4, 0x8F, 0xBB, 0xF5,
        0x78, 0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4, 0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B,
        0xE4, 0xF5, 0xCB, 0xF4, 0xD7, 0x00, 0xFC, 0xD0, 0xF3, 0xAB, 0x01, 0x10, 0xEF, 0x7E, 0xF4,
        0x10, 0xEB, 0xBA, 0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD, 0x5D, 0xD0, 0xDB, 0x1F,
        0x00, 0x00, 0xC0, 0xFF,
    ];

    pub fn new(verbose: bool) -> Self {
        let ports = Rc::new(RefCell::new(ApuPorts {
            cpu: [0; 4],
            apu: [0; 4],
            trace: false,
        }));
        Self {
            cpu: CpuSpc700::<Apubus>::new(
                Apubus::new(&Self::IPL_BIN, Rc::clone(&ports)),
                Self::IPL_ENTRYPOINT,
            ),
            spc_cycles_taken: 0,
            spc_master_credit: 0,
            verbose,
            ports,
        }
    }

    /// Get a (reference counted) copy of the communication ports
    pub fn get_ports(&self) -> Rc<RefCell<ApuPorts>> {
        Rc::clone(&self.ports)
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
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                0x2140..=0x217F => {
                    let ch = (addr - 0x2140) % 4;

                    let ports = self.ports.borrow();
                    Some(ports.cpu[ch])
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                0x2140..=0x217F => {
                    let ch = (addr - 0x2140) % 4;
                    let mut ports = self.ports.borrow_mut();
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
            },
            _ => None,
        }
    }
}
