use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use anyhow::Result;
use colored::*;
use serbia::serbia;
use serde::{Deserialize, Serialize};

use crate::snes::bus::Bus;
use crate::snes::cpu_spc700::cpu::{SpcAddress, SPC_ADDRESS_MASK};
use crate::tickable::{Tickable, Ticks};

use super::dsp::dsp::Dsp;
use super::timers::{Timer, APU_TIMERS};
use super::ApuPorts;

pub const APU_RAM_SIZE: usize = 64 * 1024;
pub const APU_ROM_SIZE: usize = 64;

/// APU peripherals as they face the SPC700 audio CPU
#[serbia]
#[derive(Serialize, Deserialize)]
pub struct Apubus {
    ram: [u8; APU_RAM_SIZE],
    rom: [u8; APU_ROM_SIZE],
    ports: Arc<Mutex<ApuPorts>>,

    /// Timers
    timers: [Timer; APU_TIMERS],

    /// Bitmask of enabled timers
    /// (also 0x00F1, bit 0-2)
    timers_enabled: u8,

    /// If true, ROM is mapped to FFC0-FFFF
    rom_mapped: bool,

    #[serde(skip)]
    pub dsp: Option<RefCell<Box<Dsp>>>,

    dsp_reg: u8,
}

impl Apubus {
    pub fn new(rom: &[u8], ports: Arc<Mutex<ApuPorts>>) -> Self {
        Self {
            ram: [0; APU_RAM_SIZE],
            rom: rom.try_into().unwrap(),
            rom_mapped: true,
            ports,

            timers: [
                // Timer 0, 8KHz (div = 128)
                Timer::new(1_024_000 / 8_000),
                // Timer 1, 8KHz (div = 128)
                Timer::new(1_024_000 / 8_000),
                // Timer 2, 64KHz (div = 16)
                Timer::new(1_024_000 / 64_000),
            ],
            timers_enabled: 0,

            dsp: None,
            dsp_reg: 0,
        }
    }
}

impl Bus<SpcAddress> for Apubus {
    fn read(&self, addr: SpcAddress) -> u8 {
        match addr {
            // DSP
            0x00F2 => self.dsp_reg,
            0x00F3 => {
                let mut dsp = self.dsp.as_ref().unwrap().borrow_mut();
                dsp.get_register(self.dsp_reg)
            }

            // Ports
            0x00F4..=0x00F7 => {
                let ports = self.ports.lock().unwrap();
                ports.apu[addr as usize - 0x00F4]
            }

            // Timers
            0x00FD => self.timers[0].get_cnt(),
            0x00FE => self.timers[1].get_cnt(),
            0x00FF => self.timers[2].get_cnt(),

            // ROM (IPL)
            0xFFC0..=0xFFFF if self.rom_mapped => self.rom[addr as usize - 0xFFC0],
            _ => self.ram[addr as usize],
        }
    }

    fn write(&mut self, addr: SpcAddress, val: u8) {
        match addr {
            0x00F1 => {
                for t in 0..APU_TIMERS {
                    if (self.timers_enabled & val) & (1 << t) == 0 {
                        self.timers[t].reset();
                    }
                }

                self.timers_enabled = val & 0x03;

                if val & (1 << 4) != 0 {
                    let mut ports = self.ports.lock().unwrap();
                    ports.apu[0] = 0;
                    ports.apu[1] = 0;
                }
                if val & (1 << 5) != 0 {
                    let mut ports = self.ports.lock().unwrap();
                    ports.apu[2] = 0;
                    ports.apu[3] = 0;
                }

                if (val & (1 << 7) != 0) != self.rom_mapped {
                    self.rom_mapped = (val & (1 << 7)) != 0;
                    println!("SPC700 ROM mapped: {}", self.rom_mapped);
                }
            }

            // DSP
            0x00F2 => self.dsp_reg = val,
            0x00F3 => {
                let mut dsp = self.dsp.as_ref().unwrap().borrow_mut();
                dsp.set_register(self.dsp_reg, val)
            }

            // Ports
            0x00F4..=0x00F7 => {
                let mut ports = self.ports.lock().unwrap();
                if ports.trace {
                    println!(
                        "{} ({:04X}) to {} ({}): {:02X}",
                        "APU".red(),
                        addr,
                        "CPU".green(),
                        (addr - 0x00F4),
                        val
                    );
                }
                ports.cpu[addr as usize - 0x00F4] = val;
            }
            0x00FA => self.timers[0].set_top(val),
            0x00FB => self.timers[1].set_top(val),
            0x00FC => self.timers[2].set_top(val),
            _ => (),
        }

        // Writes ALWAYS go through to RAM
        self.ram[addr as usize] = val;
    }

    fn get_mask(&self) -> SpcAddress {
        SPC_ADDRESS_MASK
    }

    fn get_clr_nmi(&mut self) -> bool {
        // TODO refactor this trait to remove this
        unreachable!()
    }

    fn get_clr_int(&mut self) -> bool {
        // TODO refactor this trait to remove this
        unreachable!()
    }
}

impl Tickable for Apubus {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // This ticks at the speed of the APU CPU,
        // not the CPU clock!

        for t in 0..APU_TIMERS {
            if self.timers_enabled & (1 << t) != 0 {
                self.timers[t].tick(ticks);
            }
        }
        Ok(())
    }
}
