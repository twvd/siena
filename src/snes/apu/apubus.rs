use std::cell::RefCell;
use std::rc::Rc;

use anyhow::Result;

use crate::snes::bus::{Bus, BusMember};
use crate::snes::cpu_spc700::cpu::{SpcAddress, SPC_ADDRESS_MASK};
use crate::tickable::{Tickable, Ticks};

use super::ApuPorts;

const APU_RAM_SIZE: usize = 64 * 1024;
const APU_ROM_SIZE: usize = 64;

/// APU peripherals as they face the SPC700 audio CPU
pub struct Apubus {
    ram: [u8; APU_RAM_SIZE],
    rom: [u8; APU_ROM_SIZE],
    ports: Rc<RefCell<ApuPorts>>,
}

impl Apubus {
    pub fn new(rom: &[u8], ports: Rc<RefCell<ApuPorts>>) -> Self {
        Self {
            ram: [0; APU_RAM_SIZE],
            rom: rom.try_into().unwrap(),
            ports,
        }
    }
}

impl Bus<SpcAddress> for Apubus {
    fn read(&self, addr: SpcAddress) -> u8 {
        match addr {
            // Ports
            0x00F4..=0x00F7 => {
                let ports = self.ports.borrow();
                ports.apu[addr as usize - 0x00F4]
            }

            // ROM (IPL)
            // TODO mask setting!
            0xFFC0..=0xFFFF => self.rom[addr as usize - 0xFFC0],
            _ => self.ram[addr as usize],
        }
    }

    fn write(&mut self, addr: SpcAddress, val: u8) {
        match addr {
            0x00F1 => {
                println!("APU control: {:02X}", val);

                if val & (1 << 4) != 0 {
                    let mut ports = self.ports.borrow_mut();
                    println!("Clear input 0, 1");
                    ports.apu[0] = 0;
                    ports.apu[1] = 0;
                }
                if val & (1 << 5) != 0 {
                    let mut ports = self.ports.borrow_mut();
                    println!("Clear input 2, 3");
                    ports.apu[2] = 0;
                    ports.apu[3] = 0;
                }
            }
            // Ports
            0x00F4..=0x00F7 => {
                let mut ports = self.ports.borrow_mut();
                println!("APU port {:04X} to {:02X}", addr, val);
                ports.cpu[addr as usize - 0x00F4] = val;
            }
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
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        // This ticks at the speed of the APU CPU,
        // not the master clock!
        Ok(())
    }
}
