use super::cartridge::Cartridge;
use crate::gameboy::bus::bus::BusMember;

pub struct RomOnly {
    rom: [u8; 32 * 1024],
}

impl RomOnly {
    const ROM_SIZE: usize = 32 * 1024;

    pub fn new(rom: &[u8]) -> Self {
        let mut cart = Self {
            rom: [0; Self::ROM_SIZE],
        };
        cart.rom.copy_from_slice(rom);
        cart
    }
}

impl Cartridge for RomOnly {
    fn dump_state(&self) -> String {
        "".to_string()
    }

    fn get_save(&self) -> Vec<u8> {
        vec![]
    }
}

impl BusMember for RomOnly {
    fn read(&self, addr: u16) -> u8 {
        if addr as usize > self.rom.len() {
            0xFF
        } else {
            self.rom[addr as usize]
        }
    }

    fn write(&mut self, _addr: u16, _val: u8) {}
}
