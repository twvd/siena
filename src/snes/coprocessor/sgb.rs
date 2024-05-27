use std::cell::Cell;
use std::fs;

use anyhow::Result;

use crate::bus::{Address, BusMember};
use crate::cpu_sm83::cpu::CpuSm83;
use crate::gameboy::bus::gbbus::Gameboybus;
use crate::gameboy::cartridge::cartridge;
use crate::gameboy::lcd::LCDController;
use crate::tickable::{Tickable, Ticks};

/// Super Gameboy co-processor
pub struct SuperGameboy {
    pub rownr: Cell<u16>,
    pub buffernr: Cell<u8>,

    pub cpu: CpuSm83<Gameboybus>,
}

impl SuperGameboy {
    pub fn new() -> Result<Self> {
        let rom_game = fs::read("rom/test.gb")?;
        let rom_boot = fs::read("sgb_boot.bin")?;

        let cart = cartridge::load(&rom_game);
        println!("Loaded Gameboy cartridge: {}", cart);

        let lcd = LCDController::new(false);
        let bus = Gameboybus::new(cart, Some(&rom_boot), lcd, false);
        let cpu = CpuSm83::new(bus, false);

        Ok(Self {
            rownr: Cell::new(0x11 << 4),
            buffernr: Cell::new(0),
            cpu,
        })
    }
}

impl Tickable for SuperGameboy {
    fn tick(&mut self, _ticks: Ticks) -> Result<Ticks> {
        self.cpu.step()
    }
}

impl BusMember<Address> for SuperGameboy {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x6000 => {
                self.rownr.set((self.rownr.get() + 1) % 0x120);
                if self.rownr.get() == 0 {
                    self.buffernr.set((self.buffernr.get() + 1) % 4);
                }
                println!("read row: {:X}", self.rownr.get() >> 4);
                Some((((self.rownr.get() >> 4) as u8) << 3) | (self.buffernr.get()))
            }
            // Packet available flag
            0x6002 => Some(0),
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match addr {
            0x6003 => {
                if val & 0x80 != 0 {
                    self.rownr.set(0);
                }
                Some(())
            }
            _ => None,
        }
    }
}
