use std::fmt;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::snes::bus::{Address, BusMember};

const HDR_TITLE_OFFSET: usize = 0x00;
const HDR_TITLE_SIZE: usize = 21;
const HDR_MAPMODE_OFFSET: usize = 0x15;
const HDR_CHIPSET_OFFSET: usize = 0x16;
const HDR_ROMSIZE_OFFSET: usize = 0x17;
const HDR_RAMSIZE_OFFSET: usize = 0x18;
const HDR_CHECKSUM_OFFSET: usize = 0x1C;
const HDR_ICHECKSUM_OFFSET: usize = 0x1E;
const HDR_LEN: usize = 0x1F;

#[derive(Debug, Clone, Copy, Eq, PartialEq, FromPrimitive)]
pub enum Chipset {
    RomOnly = 0,
    RomRam = 1,
    RomRamBat = 2,
    RomCo = 3,
    RomRamCo = 4,
    RomRamCoBat = 5,
    RomCoBat = 6,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, FromPrimitive)]
pub enum MapMode {
    LoROM = 0,
    HiROM = 1,
    ExHiROM = 5,
}

/// A mounted SNES cartridge
pub struct Cartridge {
    rom: Vec<u8>,
    ram: Vec<u8>,
    header_offset: usize,

    /// True if HiROM. Cache this for performance reasons.
    hirom: bool,
}

impl Cartridge {
    /// Returns the title of the cartridge as set in the header.
    pub fn get_title(&self) -> String {
        String::from_utf8(
            self.rom[(self.header_offset + HDR_TITLE_OFFSET)
                ..(self.header_offset + HDR_TITLE_OFFSET + HDR_TITLE_SIZE)]
                .into_iter()
                .take_while(|&&c| c != 0)
                .copied()
                .collect(),
        )
        .unwrap_or("INVALID".to_string())
        .trim()
        .to_owned()
    }

    fn get_map(&self) -> MapMode {
        MapMode::from_u8(self.rom[self.header_offset + HDR_MAPMODE_OFFSET] & 0x0F).unwrap()
    }

    fn get_chipset(&self) -> Chipset {
        Chipset::from_u8(self.rom[self.header_offset + HDR_CHIPSET_OFFSET] & 0x0F).unwrap()
    }

    fn get_rom_size(&self) -> usize {
        (1 << self.rom[self.header_offset + HDR_ROMSIZE_OFFSET]) * 1024
    }

    fn get_ram_size(&self) -> usize {
        (1 << self.rom[self.header_offset + HDR_RAMSIZE_OFFSET]) * 1024
    }

    fn probe_header(hdr: &[u8]) -> bool {
        let csum1: u16 =
            (hdr[HDR_CHECKSUM_OFFSET + 0] as u16) | (hdr[HDR_CHECKSUM_OFFSET + 1] as u16) << 8;
        let csum2: u16 =
            (hdr[HDR_ICHECKSUM_OFFSET + 0] as u16) | (hdr[HDR_ICHECKSUM_OFFSET + 1] as u16) << 8;
        return csum1 == (csum2 ^ 0xFFFF);
    }

    /// Loads a cartridge.
    /// Fails if it cannot find the cartridge header.
    pub fn load(rom: &[u8]) -> Self {
        Self::load_with_save(rom, &[])
    }

    /// Loads a cartridge and a save.
    /// Fails if it cannot find the cartridge header.
    pub fn load_with_save(rom: &[u8], _save: &[u8]) -> Self {
        let load_offset = match rom.len() % 1024 {
            0 => 0,
            0x200 => {
                println!("Cartridge contains 0x200 bytes of weird header");
                0x200
            }
            _ => panic!("Illogical cartridge file size: 0x{:08X}", rom.len()),
        };
        let rom = &rom[load_offset..];

        let mut header_offset = None;
        for possible_offset in [0x7FC0, 0xFFC0] {
            if (possible_offset + HDR_LEN) > rom.len() {
                continue;
            }
            if Self::probe_header(&rom[possible_offset..]) {
                println!("Cartridge header at 0x{:06X}", possible_offset);
                header_offset = Some(possible_offset);
                break;
            }
        }

        let mut c = Self {
            rom: Vec::from(rom),
            ram: vec![0; 512 * 1024],
            hirom: false,
            header_offset: header_offset.expect("Could not locate header"),
        };
        c.hirom = match c.get_map() {
            MapMode::HiROM => true,
            _ => false,
        };
        c
    }

    /// Loads a cartridge but does not do header detection
    pub fn load_nohdr(rom: &[u8], hirom: bool) -> Self {
        Self {
            rom: Vec::from(rom),
            ram: vec![0; 512 * 1024],
            hirom,
            header_offset: 0,
        }
    }

    /// Creates an empty new cartridge (for tests)
    /// Does not do header detection
    pub fn new_empty() -> Self {
        Self {
            rom: vec![],
            ram: vec![0; 512 * 1024],
            hirom: false,
            header_offset: 0,
        }
    }
}

impl fmt::Display for Cartridge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\"{}\" - {:?} {:?} - {} KB ROM, {} KB RAM",
            self.get_title(),
            self.get_chipset(),
            self.get_map(),
            self.get_rom_size() / 1024,
            self.get_ram_size() / 1024,
        )
    }
}

impl BusMember for Cartridge {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match (bank, addr) {
            // LoROM
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) => {
                Some(self.rom[addr - 0x8000 + (bank & !0x80) * 0x8000])
            }

            // HiROM SRAM
            (0x30..=0x3F, 0x6000..=0x6FFF) if self.hirom => {
                Some(self.ram[(bank - 0x30) * 0x1000 + (addr - 0x6000)])
            }

            // HiROM
            (0x40..=0x6F, _) => Some(self.rom[(addr + ((bank - 0x40) * 0x10000)) % self.rom.len()]),

            // LoROM SRAM
            (0x70..=0x7D, 0x0000..=0x7FFF) if !self.hirom => {
                Some(self.ram[(bank - 0x70) * 0x8000 + addr])
            }

            // HiROM
            (0xC0..=0xFF, _) => Some(self.rom[addr + ((bank - 0xC0) * 0x10000)]),

            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match (bank, addr) {
            // HiROM SRAM
            (0x30..=0x3F, 0x6000..=0x6FFF) if self.hirom => {
                Some(self.ram[(bank - 0x30) * 0x1000 + (addr - 0x6000)] = val)
            }

            // LoROM SRAM
            (0x70..=0x7D, 0x0000..=0x7FFF) if !self.hirom => {
                Some(self.ram[(bank - 0x70) * 0x8000 + addr] = val)
            }

            _ => None,
        }
    }
}