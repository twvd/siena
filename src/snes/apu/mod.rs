pub mod apubus;
pub mod dsp;
pub mod timers;

use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use anyhow::Result;
use colored::*;
use serde::{Deserialize, Serialize};

use crate::snes::bus::{Address, Bus, BusMember};
use crate::snes::cpu_spc700::cpu::{CpuSpc700, SpcAddress};
use crate::snes::cpu_spc700::regs::Register;
use crate::tickable::{Tickable, Ticks};

use apubus::Apubus;
use dsp::dsp::Dsp;

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
    pub ports: Arc<Mutex<ApuPorts>>,
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
        let ports = Arc::new(Mutex::new(ApuPorts {
            cpu: [0; 4],
            apu: [0; 4],
            trace: false,
        }));
        let mut apu = Self {
            cpu: CpuSpc700::<Apubus>::new(
                Apubus::new(&Self::IPL_BIN, Arc::clone(&ports)),
                Self::IPL_ENTRYPOINT,
            ),
            spc_cycles_taken: 0,
            spc_master_credit: 0,
            verbose,
            ports,
        };
        apu.cpu.bus.dsp = Some(RefCell::new(Dsp::new(&mut apu as *mut Apu)));
        apu
    }

    /// Get a (reference counted) copy of the communication ports
    pub fn get_ports(&self) -> Rc<RefCell<ApuPorts>> {
        Rc::clone(&self.ports)
    }

    /// Interface for the DSP to read I/O and RAM
    pub fn read_u8(&self, addr: u32) -> u8 {
        self.cpu.bus.read(addr as SpcAddress)
    }
    /// Interface for the DSP to read I/O and RAM
    pub fn write_u8(&mut self, addr: u32, val: u8) {
        self.cpu.bus.write(addr as SpcAddress, val)
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
                let cycles = self.cpu.step()?;
                self.spc_cycles_taken += cycles;
                self.dsp_spc_credit += cycles;

                self.cpu
                    .bus
                    .dsp
                    .as_ref()
                    .unwrap()
                    .borrow_mut()
                    .cycles_callback(cycles as i32);
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

                let ports = self.ports.lock().unwrap();
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
                let mut ports = self.ports.lock().unwrap();
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
