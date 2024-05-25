use crate::gameboy::bus::bus::BusMember;

use super::mbc1::Mbc1;
use super::mbc3::Mbc3;
use super::mbc5::Mbc5;
use super::romonly::RomOnly;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub const TITLE_OFFSET: usize = 0x134;
pub const TITLE_SIZE: usize = 16;
pub const CGB_OFFSET: usize = 0x143;
pub const CARTTYPE_OFFSET: usize = 0x147;
pub const ROMSIZE_OFFSET: usize = 0x148;
pub const RAMSIZE_OFFSET: usize = 0x149;
pub const CARTHEADER_END: usize = 0x150;

#[derive(Debug, FromPrimitive)]
pub enum CartridgeType {
    Rom = 0x00,
    Mbc1 = 0x01,
    Mbc1Ram = 0x02,
    Mbc1RamBat = 0x03,
    Mbc2 = 0x05,
    Mbc2Bat = 0x06,
    RomRam = 0x08,
    RomRamBat = 0x09,
    Mmm01 = 0x0B,
    Mmm01Ram = 0x0C,
    Mmm01RamBat = 0x0D,
    Mbc3RtcBat = 0x0F,
    Mbc3RtcRamBat = 0x10,
    Mbc3 = 0x11,
    Mbc3Ram = 0x12,
    Mbc3RamBat = 0x13,
    Mbc5 = 0x19,
    Mbc5Ram = 0x1A,
    Mbc5RamBat = 0x1B,
    Mbc5Rumble = 0x1C,
    Mbc5RumbleRam = 0x1D,
    Mbc5RumbleRamBat = 0x1E,
    Mbc6 = 0x20,
    Mbc7SensorRumbleRamBat = 0x22,
    PocketCamera = 0xFC,
    Tama5 = 0xFD,
    Huc3 = 0xFE,
    Huc1RamBat = 0xFF,
}

pub trait Cartridge: BusMember {
    fn get_title(&self) -> String {
        String::from_utf8(
            self.read_vec(TITLE_OFFSET as u16, TITLE_SIZE)
                .into_iter()
                .take_while(|&c| c != 0)
                .collect(),
        )
        .unwrap_or("INVALID".to_string())
    }

    fn get_type(&self) -> CartridgeType {
        CartridgeType::from_u8(self.read(CARTTYPE_OFFSET as u16)).unwrap()
    }

    fn get_rom_size(&self) -> usize {
        32 * 1024 * (1 << self.read(ROMSIZE_OFFSET as u16) as u32)
    }

    fn get_rom_banks(&self) -> usize {
        self.get_rom_size() / (16 * 1024)
    }

    fn get_ram_size(&self) -> usize {
        match self.read(RAMSIZE_OFFSET as u16) {
            0 => 0,
            2 => 8 * 1024,
            3 => 32 * 1024,
            4 => 128 * 1024,
            5 => 64 * 1024,
            _ => panic!(
                "Unknown RAM size value {}",
                self.read(RAMSIZE_OFFSET as u16)
            ),
        }
    }

    fn is_cgb(&self) -> bool {
        match self.read(CGB_OFFSET as u16) {
            0x80 // CGB + DMG compatible
            | 0xC0 // CGB only
            => true,
            _ => false
        }
    }

    fn get_ram_banks(&self) -> usize {
        self.get_ram_size() / (8 * 1024)
    }

    fn dump_state(&self) -> String;

    fn get_save(&self) -> Vec<u8>;
}

impl fmt::Display for dyn Cartridge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\"{}\" - {:?} - {} KB ROM ({} bank(s), 00-{:02X}), {} KB RAM ({} bank(s))",
            self.get_title(),
            self.get_type(),
            self.get_rom_size(),
            self.get_rom_banks(),
            self.get_rom_banks() - 1,
            self.get_ram_size(),
            self.get_ram_banks()
        )
    }
}

pub fn load(rom: &[u8]) -> Rc<RefCell<dyn Cartridge>> {
    load_with_save(rom, &[])
}

pub fn load_with_save(rom: &[u8], save: &[u8]) -> Rc<RefCell<dyn Cartridge>> {
    assert!(rom.len() >= 32 * 1024);

    match CartridgeType::from_u8(rom[CARTTYPE_OFFSET]) {
        Some(CartridgeType::Rom) => Rc::new(RefCell::new(RomOnly::new(rom))),
        Some(CartridgeType::Mbc1) => Rc::new(RefCell::new(Mbc1::new(rom, save))),
        Some(CartridgeType::Mbc1Ram) => Rc::new(RefCell::new(Mbc1::new(rom, save))),
        Some(CartridgeType::Mbc1RamBat) => Rc::new(RefCell::new(Mbc1::new(rom, save))),
        Some(CartridgeType::Mbc3) => Rc::new(RefCell::new(Mbc3::new(rom, save))),
        Some(CartridgeType::Mbc3Ram) => Rc::new(RefCell::new(Mbc3::new(rom, save))),
        Some(CartridgeType::Mbc3RamBat) => Rc::new(RefCell::new(Mbc3::new(rom, save))),
        Some(CartridgeType::Mbc3RtcRamBat) => Rc::new(RefCell::new(Mbc3::new(rom, save))),
        Some(CartridgeType::Mbc5) => Rc::new(RefCell::new(Mbc5::new(rom, save))),
        Some(CartridgeType::Mbc5Ram) => Rc::new(RefCell::new(Mbc5::new(rom, save))),
        Some(CartridgeType::Mbc5RamBat) => Rc::new(RefCell::new(Mbc5::new(rom, save))),
        Some(CartridgeType::Mbc5RumbleRamBat) => Rc::new(RefCell::new(Mbc5::new(rom, save))),
        Some(unknown) => panic!("Unknown cartridge type {:?}", unknown),
        _ => panic!("Unknown cartridge type {:02X}", rom[CARTTYPE_OFFSET]),
    }
}
