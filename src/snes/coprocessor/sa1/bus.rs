use anyhow::Result;
use serbia::serbia;
use serde::{Deserialize, Serialize};

use super::{BWRAM_SIZE, IRAM_SIZE};
use crate::bus::{Address, Bus, ADDRESS_MASK};
use crate::tickable::{Tickable, Ticks};

/// Peripherals as they face the SA-1 65816
#[serbia]
#[derive(Serialize, Deserialize)]
pub struct Sa1Bus {
    pub rom: Vec<u8>,
    pub rom_mask: usize,
    pub bwram: Vec<u8>,
    pub iram: Vec<u8>,

    /// SA-1 CPU control
    pub sa1_cpu_ctrl: u8,
    /// SA-1 CPU Reset vector
    pub sa1_crv: Address,
    /// SA-1 CPU NMI vector
    pub sa1_cnv: Address,
    /// SA-1 CPU IRQ vector
    pub sa1_civ: Address,
}

impl Sa1Bus {
    pub fn new(rom: Vec<u8>, rom_mask: usize) -> Self {
        Self {
            rom,
            rom_mask,
            bwram: vec![0; BWRAM_SIZE],
            iram: vec![0; IRAM_SIZE],

            sa1_cpu_ctrl: 0x20,
            sa1_crv: 0,
            sa1_cnv: 0,
            sa1_civ: 0,
        }
    }
}

impl Bus<Address> for Sa1Bus {
    fn read(&self, fulladdr: Address) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        // Note: SA-1 has a flexible mapper and most of the SNES memory map matches the SA-1's,
        // so any cartridge access is forwarded here.
        let val = match (bank, addr) {
            // I/O ports
            (0x00..=0x3F | 0x80..=0xBF, 0x2200..=0x23FF) => None,

            // I-RAM (not re-mappable, SA-1 only!)
            (0x00..=0x3F | 0x80..=0xBF, 0x0000..=0x07FF) => Some(self.iram[addr]),

            // I-RAM (not re-mappable)
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x37FF) => Some(self.iram[addr - 0x3000]),

            // BW-RAM (mappable 8K block)
            // TODO MMC mapping
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) => Some(self.bwram[addr - 0x6000]),

            // LoROM (mappable)
            // TODO MMC mapping
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) => {
                Some(self.rom[(addr - 0x8000 + (bank & !0x80) * 0x8000) & self.rom_mask])
            }

            // BW-RAM (not re-mappable)
            (0x40..=0x4F, _) => Some(self.bwram[addr + ((bank & 0x03) * 0x10000)]),

            // BW-RAM pixel buffer (SA-1 only!)
            (0x60..=0x6F, _) => todo!(),

            // HiROM
            // TODO MMC mapping
            (0xC0..=0xFF, _) => Some(self.rom[(addr + ((bank - 0xC0) * 0x10000)) & self.rom_mask]),

            _ => None,
        };

        if let Some(v) = val {
            v
        } else {
            // TODO open bus
            println!("SA-1 open bus read: {:06X}", fulladdr);
            0xFF
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        // Note: SA-1 has a flexible mapper and most of the SNES memory map matches the SA-1's,
        // so any cartridge access is forwarded here.
        match (bank, addr) {
            // I/O ports
            (0x00..=0x3F | 0x80..=0xBF, 0x2200..=0x23FF) => match addr {
                // SNES CCNT - SA-1 CPU Control
                0x2200 => self.sa1_cpu_ctrl = val,
                // SNES CRV - SA-1 CPU Reset Vector
                0x2203 => self.sa1_crv = Address::from(val) | (self.sa1_crv & 0xFF00),
                0x2204 => self.sa1_crv = (Address::from(val) << 8) | (self.sa1_crv & 0xFF),
                // SNES CNV - SA-1 CPU NMI Vector
                0x2205 => self.sa1_cnv = Address::from(val) | (self.sa1_cnv & 0xFF00),
                0x2206 => self.sa1_cnv = (Address::from(val) << 8) | (self.sa1_cnv & 0xFF),
                // SNES CIV - SA-1 CPU IRQ Vector
                0x2207 => self.sa1_civ = Address::from(val) | (self.sa1_civ & 0xFF00),
                0x2208 => self.sa1_civ = (Address::from(val) << 8) | (self.sa1_civ & 0xFF),
                _ => println!(
                    "SA-1 unimplemented I/O write {:06X} = {:02X}",
                    fulladdr, val
                ),
            },

            // I-RAM (not re-mappable, SA-1 only!)
            (0x00..=0x3F | 0x80..=0xBF, 0x0000..=0x07FF) => self.iram[addr] = val,

            // I-RAM (not re-mappable)
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x37FF) => self.iram[addr - 0x3000] = val,

            // BW-RAM (mappable 8K block)
            // TODO MMC mapping
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) => self.bwram[addr - 0x6000] = val,

            // BW-RAM (not re-mappable)
            (0x40..=0x4F, _) => self.bwram[addr + ((bank & 0x03) * 0x10000)] = val,

            // BW-RAM pixel buffer (SA-1 only!)
            (0x60..=0x6F, _) => todo!(),

            _ => println!("SA-1 open bus write: {:06X} = {:02X}", fulladdr, val),
        };
    }

    fn get_mask(&self) -> Address {
        ADDRESS_MASK
    }

    fn get_clr_nmi(&mut self) -> bool {
        false
    }

    fn get_int(&mut self) -> bool {
        false
    }
}

impl Tickable for Sa1Bus {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        Ok(ticks)
    }
}
