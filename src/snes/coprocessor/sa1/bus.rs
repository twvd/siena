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

    mcnt: u8,
    ma: u16,
    mb: u16,
    mr: u64,
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

            mcnt: 0,
            ma: 0,
            mb: 0,
            mr: 0,
        }
    }

    fn arithmetic_start(&mut self) {
        match self.mcnt & 0x03 {
            // Multiply
            0 => self.mr = ((self.ma as i16 as i32) * (self.mb as i16 as i32)) as u64,
            // Division
            1 => {
                let quotient = (self.ma as i16 as i32) / (self.mb as i32);
                let remainder = (self.ma as i16 as i32) % (self.mb as i32);
                self.mr = (quotient as u64) | ((remainder as u64) << 16);
            }
            // MultiplySum
            2 => self.mr += ((self.ma as i16 as i32) * (self.mb as i16 as i32)) as u64,
            // Reserved
            3 => unreachable!(),

            _ => unreachable!(),
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
            (0x00..=0x3F | 0x80..=0xBF, 0x2200..=0x23FF) => match addr {
                // SA-1 MR - Arithmetic Result
                0x2306 => Some((self.mr >> 0) as u8),
                0x2307 => Some((self.mr >> 8) as u8),
                0x2308 => Some((self.mr >> 16) as u8),
                0x2309 => Some((self.mr >> 24) as u8),
                0x230A => Some((self.mr >> 32) as u8),

                _ => None,
            },

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

                // SA-1 MCNT - Arithmetic Control
                0x2250 => {
                    self.mcnt = val;
                    if val & 1 != 0 {
                        self.mr = 0;
                    }
                }
                // SA-1 MA - Arithmetic Parameter A
                0x2251 => self.ma = u16::from(val) | (self.ma & 0xFF00),
                0x2252 => self.ma = (u16::from(val) << 8) | (self.ma & 0xFF),
                // SA-1 MB - Arithmetic Parameter B
                0x2253 => self.mb = u16::from(val) | (self.mb & 0xFF00),
                0x2254 => {
                    self.mb = (u16::from(val) << 8) | (self.mb & 0xFF);
                    self.arithmetic_start();
                }

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
