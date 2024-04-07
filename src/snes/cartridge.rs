use std::fmt;

use anyhow::{anyhow, Result};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};
use strum::Display;

use super::coprocessor::dsp1::DSP1;
use super::coprocessor::superfx::SuperFX;

use crate::bus::{Address, BusMember};
use crate::cpu_gsu::cpu::GsuMap;

const HDR_TITLE_OFFSET: usize = 0x00;
const HDR_TITLE_SIZE: usize = 21;
const HDR_MAPMODE_OFFSET: usize = 0x15;
const HDR_CHIPSET_OFFSET: usize = 0x16;
const HDR_ROMSIZE_OFFSET: usize = 0x17;
const HDR_RAMSIZE_OFFSET: usize = 0x18;
const HDR_DESTINATION_OFFSET: usize = 0x19;
const HDR_CHECKSUM_OFFSET: usize = 0x1C;
const HDR_ICHECKSUM_OFFSET: usize = 0x1E;
const HDR_LEN: usize = 0x1F;
const RAM_SIZE: usize = 0x20000;

#[derive(Copy, Clone, Display, Serialize, Deserialize, clap::ValueEnum)]
pub enum VideoFormat {
    PAL,
    NTSC,
}

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
pub enum CoProcessor {
    DSPx = 0,
    SuperFX = 1,
    OBC1 = 2,
    SA1 = 3,
    SDD1 = 4,
    SRTC = 5,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, FromPrimitive)]
pub enum MapMode {
    LoROM = 0,
    HiROM = 1,
    ExHiROM = 5,
}

#[derive(
    Debug,
    Clone,
    Copy,
    Eq,
    PartialEq,
    FromPrimitive,
    Serialize,
    Deserialize,
    Display,
    clap::ValueEnum,
)]
pub enum Mapper {
    LoROM,
    HiROM,
    LoROMDSP1,
    HiROMDSP1,
    SuperFXMC1,
    SuperFX1,
    SuperFX2,
}

/// A mounted SNES cartridge
#[derive(Serialize, Deserialize)]
pub struct Cartridge {
    rom: Vec<u8>,
    ram: Vec<u8>,
    header_offset: usize,

    /// Mapper implementation to use
    mapper: Mapper,

    /// RAM address mask, to properly emulate mirroring
    /// 0 == no RAM
    ram_mask: usize,

    /// ROM address mask
    rom_mask: usize,

    /// DSP-1 co-processor
    pub co_dsp1: Option<DSP1>,

    /// SuperFX co-processor
    pub co_superfx: Option<SuperFX>,
}

impl Cartridge {
    fn parse_title(s: &[u8]) -> Result<String> {
        String::from_utf8(s.into_iter().take_while(|&&c| c != 0).copied().collect())
            .map_err(|e| anyhow!(e))
    }

    /// Returns the title of the cartridge as set in the header.
    pub fn get_title(&self) -> String {
        Self::parse_title(
            &self.rom[(self.header_offset + HDR_TITLE_OFFSET)
                ..(self.header_offset + HDR_TITLE_OFFSET + HDR_TITLE_SIZE)],
        )
        .unwrap_or("UNKNOWN".to_string())
        .trim()
        .to_owned()
    }

    /// Gets the title, cleaned up to be ASCII without whitespace
    pub fn get_title_clean(&self) -> String {
        self.get_title()
            .chars()
            .filter(|&c| c.is_ascii())
            .map(|c| {
                if c.is_whitespace() {
                    '_'
                } else {
                    c.to_ascii_lowercase()
                }
            })
            .collect()
    }

    fn get_map(&self) -> MapMode {
        MapMode::from_u8(self.rom[self.header_offset + HDR_MAPMODE_OFFSET] & 0x0F).unwrap()
    }

    fn get_chipset(&self) -> Chipset {
        if self.rom[self.header_offset + HDR_CHIPSET_OFFSET] & 0x0F == 0x0A {
            Chipset::RomRamCoBat
        } else {
            Chipset::from_u8(self.rom[self.header_offset + HDR_CHIPSET_OFFSET] & 0x0F).unwrap()
        }
    }

    fn get_rom_size(&self) -> usize {
        (1 << self.rom[self.header_offset + HDR_ROMSIZE_OFFSET]) * 1024
    }

    fn get_ram_size(&self) -> usize {
        match self.get_chipset() {
            Chipset::RomOnly | Chipset::RomCo | Chipset::RomCoBat => 0,
            _ => (1 << self.rom[self.header_offset + HDR_RAMSIZE_OFFSET]) * 1024,
        }
    }

    fn get_coprocessor(&self) -> Option<CoProcessor> {
        match self.get_chipset() {
            Chipset::RomCo | Chipset::RomRamCo | Chipset::RomRamCoBat | Chipset::RomCoBat => Some(
                CoProcessor::from_u8(self.rom[self.header_offset + HDR_CHIPSET_OFFSET] >> 4)
                    .unwrap(),
            ),
            _ => None,
        }
    }

    fn has_ram(&self) -> bool {
        self.ram_mask != 0
    }

    pub fn get_video_format(&self) -> VideoFormat {
        match self.rom[self.header_offset + HDR_DESTINATION_OFFSET] {
            0x00 // Japan
            | 0x01 // North-America
            | 0x0D // South Korea
            | 0x0F // Canada
            => VideoFormat::NTSC,
            _ => VideoFormat::PAL
        }
    }

    fn probe_header(hdr: &[u8]) -> bool {
        let csum1: u16 =
            (hdr[HDR_CHECKSUM_OFFSET + 0] as u16) | (hdr[HDR_CHECKSUM_OFFSET + 1] as u16) << 8;
        let csum2: u16 =
            (hdr[HDR_ICHECKSUM_OFFSET + 0] as u16) | (hdr[HDR_ICHECKSUM_OFFSET + 1] as u16) << 8;
        return csum1 == (csum2 ^ 0xFFFF)
            && Self::parse_title(&hdr[HDR_TITLE_OFFSET..(HDR_TITLE_OFFSET + HDR_TITLE_SIZE)])
                .is_ok();
    }

    /// Loads a cartridge.
    /// Fails if it cannot find the cartridge header.
    pub fn load(rom: &[u8], co_rom: Option<&[u8]>) -> Self {
        Self::load_with_save(rom, &[], co_rom)
    }

    /// Loads a cartridge and a save.
    /// Fails if it cannot find the cartridge header.
    pub fn load_with_save(rom: &[u8], _save: &[u8], co_rom: Option<&[u8]>) -> Self {
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
            ram: vec![0; RAM_SIZE],
            header_offset: header_offset.expect("Could not locate header"),
            ram_mask: 0,
            rom_mask: 0,
            co_dsp1: None,
            co_superfx: None,
            mapper: Mapper::LoROM,
        };

        // Detect / initialize co-processor
        match c.get_coprocessor() {
            Some(CoProcessor::DSPx) => {
                println!("DSP-1 co-processor detected");
                if let Some(rom) = co_rom {
                    c.co_dsp1 = Some(DSP1::new());
                    c.co_dsp1.as_mut().unwrap().load_rom_combined(rom);
                } else {
                    panic!("DSP-1 co-processor requires a ROM, please specify using --corom");
                }
                // TODO detect DSP-2, DSP-3, DSP-4
            }
            Some(CoProcessor::SuperFX) => {
                println!("SuperFX co-processor detected");
                let (sfx_map, map, ram_mask) = match c.get_title().as_str() {
                    "DIRT RACER" => (GsuMap::SuperFX1, Mapper::SuperFX1, 0x1FFFF),
                    "DIRT TRAX FX" => (GsuMap::SuperFX1, Mapper::SuperFX1, 0x1FFFF),
                    "DOOM" => (GsuMap::SuperFX2, Mapper::SuperFX2, 0xFFFF),
                    "FX SKIING NINTENDO 96" => (GsuMap::SuperFX2, Mapper::SuperFX2, 0xFFFF),
                    "STAR FOX" => (GsuMap::SuperFX1, Mapper::SuperFXMC1, 0x7FFF),
                    "STARFOX2" => (GsuMap::SuperFX2, Mapper::SuperFX2, 0xFFFF),
                    "Stunt Race FX" => (GsuMap::SuperFX1, Mapper::SuperFX1, 0xFFFFF),
                    "SUPER FX TEST" => (GsuMap::SuperFX2, Mapper::SuperFX2, 0xFFFFF),
                    "VORTEX" => (GsuMap::SuperFX1, Mapper::SuperFX1, 0x7FFF),
                    "YOSHI'S ISLAND" => (GsuMap::SuperFX2, Mapper::SuperFX2, 0x7FFF),
                    _ => panic!("Unknown SuperFX game \"{}\"", c.get_title()),
                };
                println!(
                    "Cartridge map {:?}, GSU map {:?}, shared RAM mask {:06X}",
                    map, sfx_map, ram_mask
                );
                c.co_superfx = Some(SuperFX::new(rom, sfx_map, ram_mask));
                c.mapper = map;
            }
            Some(c) => println!("Warning: unimplemented co-processor: {:?}", c),
            None => (),
        }

        // TODO refactor header to its own struct
        c.mapper = match (c.get_map(), c.get_coprocessor()) {
            (MapMode::LoROM, None) => Mapper::LoROM,
            (MapMode::HiROM, None) => Mapper::HiROM,
            (MapMode::LoROM, Some(CoProcessor::DSPx)) => Mapper::LoROMDSP1,
            (MapMode::HiROM, Some(CoProcessor::DSPx)) => Mapper::HiROMDSP1,
            (_, Some(CoProcessor::SuperFX)) => c.mapper,
            _ => panic!("Cannot determine mapper"),
        };
        println!("Selected mapper: {}", c.mapper);
        if c.get_ram_size() > 0 {
            c.ram_mask = c.get_ram_size() - 1;
        }
        c.rom_mask = c.get_rom_size() - 1;
        println!(
            "ROM mask: {:06X} - RAM mask: {:06X}",
            c.rom_mask, c.ram_mask
        );
        c
    }

    /// Loads a cartridge but does not do header detection
    pub fn load_nohdr(rom: &[u8], mapper: Mapper) -> Self {
        println!("Selected mapper: {}", mapper);
        Self {
            rom: Vec::from(rom),
            ram: vec![0; RAM_SIZE],
            mapper: mapper,
            header_offset: 0,
            ram_mask: RAM_SIZE - 1,
            rom_mask: rom.len() - 1,
            co_dsp1: None,
            co_superfx: if mapper == Mapper::SuperFX1 {
                Some(SuperFX::new(rom, GsuMap::SuperFX1, 0x1FFFF))
            } else {
                None
            },
        }
    }

    /// Creates an empty new cartridge (for tests)
    /// Does not do header detection
    pub fn new_empty() -> Self {
        Self {
            rom: vec![],
            ram: vec![0; RAM_SIZE],
            mapper: Mapper::LoROM,
            header_offset: 0,
            ram_mask: RAM_SIZE - 1,
            rom_mask: usize::MAX,
            co_dsp1: None,
            co_superfx: None,
        }
    }

    fn read_lorom(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            (0x00..=0x3F | 0x80..=0xFF, 0x8000..=0xFFFF) => {
                Some(self.rom[addr - 0x8000 + (bank & !0x80) * 0x8000])
            }
            (0x70..=0x7D, 0x0000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(bank - 0x70) * 0x8000 + addr & self.ram_mask])
            }
            _ => None,
        }
    }

    fn read_lorom_dsp(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            (0x00..=0x1F | 0x80..=0x9F, 0x8000..=0xFFFF) => {
                Some(self.rom[addr - 0x8000 + (bank & !0x80) * 0x8000])
            }
            (0x70..=0x7D, 0x0000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(bank - 0x70) * 0x8000 + addr & self.ram_mask])
            }

            // DSP-1 co-processor
            (0x30..=0x3F | 0xB0..=0xBF, 0x8000..=0xBFFF) => {
                let dsp = self.co_dsp1.as_ref().unwrap();
                Some(dsp.read_dr())
            }
            (0x30..=0x3F | 0xB0..=0xBF, 0xC000..=0xFFFF) => {
                let dsp = self.co_dsp1.as_ref().unwrap();
                Some(dsp.read_sr())
            }

            _ => None,
        }
    }

    fn read_superfx_mc1(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match (bank, addr) {
            // LoROM
            (0x00..=0x3F | 0x80..=0xFF, 0x8000..=0xFFFF) => {
                Some(self.rom[(addr - 0x8000 + (bank & !0x80) * 0x8000) & self.rom_mask])
            }

            // Shared SRAM
            (0x60..=0x7D | 0xE0..=0xFF, 0x0000..=0xFFFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let cpu = sfx.cpu.borrow();
                Some(cpu.ram[((bank & 0x1F) * 0x10000 + addr) & cpu.ram_mask])
            }

            // SuperFX co-processor
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x347F) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                sfx.read(fulladdr)
            }
            _ => None,
        }
    }

    fn read_superfx1(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match (bank, addr) {
            // LoROM
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) => {
                Some(self.rom[(addr - 0x8000 + (bank & !0x80) * 0x8000) & self.rom_mask])
            }

            // HiROM
            (0x40..=0x5F, _) => Some(self.rom[(addr + ((bank - 0x40) * 0x10000)) & self.rom_mask]),
            (0xC0..=0xDF, _) => Some(self.rom[(addr + ((bank - 0xC0) * 0x10000)) & self.rom_mask]),

            // Shared SRAM
            (0x70..=0x71 | 0xF0..=0xF1, 0x0000..=0xFFFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let cpu = sfx.cpu.borrow();
                Some(cpu.ram[((bank & 1) * 0x10000 + addr) & cpu.ram_mask])
            }

            // SuperFX co-processor
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x34FF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                sfx.read(fulladdr)
            }
            _ => None,
        }
    }

    fn read_superfx2(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match (bank, addr) {
            // LoROM
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) => {
                Some(self.rom[(addr - 0x8000 + (bank & !0x80) * 0x8000) & self.rom_mask])
            }

            // HiROM
            (0x40..=0x5F, _) => Some(self.rom[(addr + ((bank - 0x40) * 0x10000)) & self.rom_mask]),

            // Shared SRAM
            (0x70..=0x71, 0x0000..=0xFFFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let cpu = sfx.cpu.borrow();
                Some(cpu.ram[(((bank & !0x80) - 0x70) * 0x10000 + addr) & cpu.ram_mask])
            }

            // Shared RAM mirror
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let cpu = sfx.cpu.borrow();
                Some(cpu.ram[addr - 0x6000])
            }

            // Backup RAM
            (0x78..=0x79, 0x0000..=0xFFFF) => Some(self.ram[(bank - 0x78) * 0x10000 + addr]),

            // SuperFX co-processor
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x34FF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                sfx.read(fulladdr)
            }
            _ => None,
        }
    }

    fn write_lorom(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // LoROM SRAM
            (0x70..=0x7D, 0x0000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(bank - 0x70) * 0x8000 + addr & self.ram_mask] = val)
            }

            _ => None,
        }
    }

    fn write_lorom_dsp(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // LoROM SRAM
            (0x70..=0x7D, 0x0000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(bank - 0x70) * 0x8000 + addr & self.ram_mask] = val)
            }

            // DSP-1 co-processor
            (0x30..=0x3F | 0xB0..=0xBF, 0x8000..=0xBFFF) => {
                let dsp = self.co_dsp1.as_mut().unwrap();
                Some(dsp.write_dr(val))
            }

            _ => None,
        }
    }

    fn write_superfx_mc1(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // Shared SRAM
            (0x60..=0x7D | 0xE0..=0xFF, 0x0000..=0xFFFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let mut cpu = sfx.cpu.borrow_mut();
                let mask = cpu.ram_mask;

                Some(cpu.ram[((bank & 0x1F) * 0x10000 + addr) & mask] = val)
            }

            // SuperFX co-processor
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x347F) => {
                let sfx = self.co_superfx.as_mut().unwrap();
                sfx.write(fulladdr, val)
            }

            _ => None,
        }
    }

    fn write_superfx1(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // Shared SRAM
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let mut cpu = sfx.cpu.borrow_mut();
                Some(cpu.ram[addr - 0x6000] = val)
            }
            (0x70..=0x71, 0x0000..=0xFFFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let mut cpu = sfx.cpu.borrow_mut();
                let mask = cpu.ram_mask;
                Some(cpu.ram[((bank & 1) * 0x10000 + addr) & mask] = val)
            }

            // SuperFX co-processor
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x34FF) => {
                let sfx = self.co_superfx.as_mut().unwrap();
                sfx.write(fulladdr, val)
            }

            _ => None,
        }
    }

    fn write_superfx2(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // Shared SRAM
            (0x70..=0x71, 0x0000..=0xFFFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let mut cpu = sfx.cpu.borrow_mut();
                let mask = cpu.ram_mask;
                Some(cpu.ram[(((bank & !0x80) - 0x70) * 0x10000 + addr) & mask] = val)
            }

            // RAM
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) => {
                let sfx = self.co_superfx.as_ref().unwrap();
                let mut cpu = sfx.cpu.borrow_mut();
                Some(cpu.ram[addr - 0x6000] = val)
            }

            // Backup RAM
            (0x78..=0x79, _) => Some(self.ram[(bank - 0x78) * 0x10000 + addr] = val),

            // SuperFX co-processor
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x34FF) => {
                let sfx = self.co_superfx.as_mut().unwrap();
                sfx.write(fulladdr, val)
            }

            _ => None,
        }
    }

    fn read_hirom(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // HiROM (mirrors in LoROM banks)
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) => {
                Some(self.rom[(addr - 0x0000 + (bank & !0x80) * 0x10000) & self.rom_mask])
            }

            // HiROM SRAM
            (0x30..=0x3F, 0x6000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[((bank - 0x30) * 0x2000 + (addr - 0x6000)) & self.ram_mask])
            }
            (0x80..=0xBF, 0x6000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(((bank - 0x80) + 0x20) * 0x2000 + (addr - 0x6000)) & self.ram_mask])
            }

            // HiROM
            (0x40..=0x6F, _) => Some(self.rom[(addr + ((bank - 0x40) * 0x10000)) & self.rom_mask]),

            // HiROM
            (0xC0..=0xFF, _) => Some(self.rom[(addr + ((bank - 0xC0) * 0x10000)) & self.rom_mask]),
            _ => None,
        }
    }

    fn write_hirom(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // HiROM SRAM
            (0x30..=0x3F, 0x6000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(bank - 0x30) * 0x2000 + (addr - 0x6000) & self.ram_mask] = val)
            }
            (0x80..=0xBF, 0x6000..=0x7FFF) if self.has_ram() => Some(
                self.ram[((bank - 0x80) + 0x20) * 0x2000 + (addr - 0x6000) & self.ram_mask] = val,
            ),

            _ => None,
        }
    }

    fn read_hirom_dsp(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // HiROM (mirrors in LoROM banks)
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) => {
                Some(self.rom[(addr - 0x0000 + (bank & !0x80) * 0x10000) & self.rom_mask])
            }

            // HiROM SRAM
            (0x20..=0x3F, 0x6000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[((bank - 0x20) * 0x2000 + (addr - 0x6000)) & self.ram_mask])
            }
            (0xA0..=0xBF, 0x6000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[(((bank - 0xA0) + 0x20) * 0x2000 + (addr - 0x6000)) & self.ram_mask])
            }

            // HiROM
            (0x40..=0x6F, _) => Some(self.rom[(addr + ((bank - 0x40) * 0x10000)) & self.rom_mask]),

            // HiROM
            (0xC0..=0xFF, _) => Some(self.rom[(addr + ((bank - 0xC0) * 0x10000)) & self.rom_mask]),

            // DSP-1 co-processor
            (0x00..=0x1F | 0x80..=0x9F, 0x6000..=0x6FFF) => {
                let dsp = self.co_dsp1.as_ref().unwrap();
                Some(dsp.read_dr())
            }
            (0x00..=0x1F | 0x80..=0x9F, 0x7000..=0x7FFF) => {
                let dsp = self.co_dsp1.as_ref().unwrap();
                Some(dsp.read_sr())
            }

            _ => None,
        }
    }

    fn write_hirom_dsp(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match (bank, addr) {
            // HiROM SRAM
            (0x20..=0x3F, 0x6000..=0x7FFF) if self.has_ram() => {
                Some(self.ram[((bank - 0x20) * 0x2000 + (addr - 0x6000)) & self.ram_mask] = val)
            }
            (0xA0..=0xBF, 0x6000..=0x7FFF) if self.has_ram() => Some(
                self.ram[(((bank - 0xA0) + 0x20) * 0x2000 + (addr - 0x6000)) & self.ram_mask] = val,
            ),

            // DSP-1 co-processor
            (0x00..=0x1F | 0x80..=0x9F, 0x6000..=0x6FFF) => {
                let dsp = self.co_dsp1.as_mut().unwrap();
                Some(dsp.write_dr(val))
            }
            _ => None,
        }
    }

    pub fn get_int(&mut self) -> bool {
        if let Some(sfx) = self.co_superfx.as_mut() {
            return sfx.get_int();
        }

        false
    }
}

impl fmt::Display for Cartridge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\"{}\" {} - {:?} {:?} - {} KB ROM, {} KB RAM",
            self.get_title(),
            self.get_video_format(),
            self.get_chipset(),
            self.get_map(),
            self.get_rom_size() / 1024,
            self.get_ram_size() / 1024,
        )
    }
}

impl BusMember<Address> for Cartridge {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        match self.mapper {
            Mapper::LoROM => self.read_lorom(fulladdr),
            Mapper::HiROM => self.read_hirom(fulladdr),
            Mapper::LoROMDSP1 => self.read_lorom_dsp(fulladdr),
            Mapper::HiROMDSP1 => self.read_hirom_dsp(fulladdr),
            Mapper::SuperFXMC1 => self.read_superfx_mc1(fulladdr),
            Mapper::SuperFX1 => self.read_superfx1(fulladdr),
            Mapper::SuperFX2 => self.read_superfx2(fulladdr),
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        match self.mapper {
            Mapper::LoROM => self.write_lorom(fulladdr, val),
            Mapper::HiROM => self.write_hirom(fulladdr, val),
            Mapper::LoROMDSP1 => self.write_lorom_dsp(fulladdr, val),
            Mapper::HiROMDSP1 => self.write_hirom_dsp(fulladdr, val),
            Mapper::SuperFXMC1 => self.write_superfx_mc1(fulladdr, val),
            Mapper::SuperFX1 => self.write_superfx1(fulladdr, val),
            Mapper::SuperFX2 => self.write_superfx2(fulladdr, val),
        }
    }
}
