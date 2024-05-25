use super::cartridge::Cartridge;
use crate::gameboy::bus::bus::BusMember;

use std::cmp;

const ROM_BANK_SIZE: usize = 16 * 1024;
const ROM_BANK_COUNT: usize = ROM_BANKS_MAX + 1;
const ROM_BANKS_MAX: usize = 127;

const RAM_BANK_SIZE: usize = 8 * 1024;
const RAM_BANK_COUNT: usize = RAM_BANKS_MAX + 1;
const RAM_BANKS_MAX: usize = 0x03;
const RAM_BANK_MASK: u8 = 0x0F;

pub struct Mbc3 {
    rom: Vec<u8>,
    rom_banksel: u8,
    ram: Vec<u8>,
    ram_banksel: u8,
}

impl Mbc3 {
    pub fn new(rom: &[u8], save: &[u8]) -> Self {
        let mut cart = Self {
            // Too large for the stack..
            rom: vec![0; ROM_BANK_COUNT * ROM_BANK_SIZE],
            ram: vec![0; RAM_BANK_COUNT * RAM_BANK_SIZE],
            rom_banksel: 1,
            ram_banksel: 0,
        };
        cart.rom[0..rom.len()].copy_from_slice(rom);
        cart.ram[0..save.len()].copy_from_slice(save);
        cart
    }

    fn rom_translate(&self, addr: u16) -> usize {
        assert!(addr >= 0x4000);

        let bankaddr: usize = ROM_BANK_SIZE * (self.rom_banksel as usize);
        bankaddr + (addr as usize - 0x4000)
    }

    fn ram_translate(&self, addr: u16) -> usize {
        assert!(addr >= 0xA000);

        let bankaddr: usize = RAM_BANK_SIZE * (self.ram_banksel as usize);
        bankaddr + (addr as usize - 0xA000)
    }
}

impl Cartridge for Mbc3 {
    fn dump_state(&self) -> String {
        format!(
            "ROM bank: {:02X} - RAM bank: {:02X}",
            self.rom_banksel, self.ram_banksel
        )
    }

    fn get_save(&self) -> Vec<u8> {
        self.ram.to_owned()
    }
}

impl BusMember for Mbc3 {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // ROM - Always bank 0
            0x0000..=0x3FFF => self.rom[addr as usize],
            // ROM - Bank 1..=127
            0x4000..=0x7FFF => self.rom[self.rom_translate(addr)],
            // RAM - Bank 0..=3
            0xA000..=0xBFFF if self.ram_banksel < RAM_BANK_COUNT as u8 => {
                self.ram[self.ram_translate(addr)]
            }
            // RTC registers
            0xA000..=0xBFFF => 0, // TODO

            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // RAM + RTC enable
            0x0000..=0x1FFF => (),
            // ROM bank select
            0x2000..=0x3FFF => self.rom_banksel = cmp::max(val, 1) & ROM_BANKS_MAX as u8,
            // RAM/upper ROM bank select
            0x4000..=0x5FFF => self.ram_banksel = val & RAM_BANK_MASK as u8,
            // RTC Latch clock data
            0x6000..=0x7FFF => (),
            // RAM - Bank 0..=3
            0xA000..=0xBFFF if self.ram_banksel < RAM_BANK_COUNT as u8 => {
                let tr_addr = self.ram_translate(addr);
                self.ram[tr_addr] = val
            }
            // RTC registers
            0xA000..=0xBFFF => (), // TODO
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use itertools::repeat_n;

    #[test]
    fn rom_bank_switching() {
        let rom: Box<Vec<u8>> = Box::new(
            (0u8..=(ROM_BANKS_MAX as u8))
                .flat_map(|i| repeat_n(i, ROM_BANK_SIZE))
                .collect(),
        );
        assert_eq!(rom.len(), ROM_BANK_COUNT * ROM_BANK_SIZE);

        let mut c = Mbc3::new(&rom, &[]);

        // Bank 0
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(i), 0);
        }
        // Bank n default (1)
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 1);
        }

        // Test each bank n
        for b in 1u8..=(ROM_BANKS_MAX as u8) {
            c.write(0x2000, b);
            // Bank 0
            for i in 0..(ROM_BANK_SIZE as u16) {
                assert_eq!(c.read(i), 0);
            }
            // Bank n
            for i in 0u16..(ROM_BANK_SIZE as u16) {
                assert_eq!(c.read(0x4000 + i), b);
            }
        }

        // Test masking
        c.write(0x2000, 0xFF);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x7F);
        }

        // Selecting bank 0 should select bank 1
        c.write(0x2000, 0);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x01);
        }
    }

    #[test]
    fn ram_bank_switching() {
        let mut c = Mbc3::new(&[], &[]);

        for b in 0u8..(RAM_BANK_COUNT as u8) {
            c.write(0x4000, b);
            for n in 0u16..(RAM_BANK_SIZE as u16) {
                assert_eq!(c.read(0xA000 + n), 0);
                c.write(0xA000 + n, b + 1);
            }
        }

        for b in 0u8..(RAM_BANK_COUNT as u8) {
            c.write(0x4000, b);
            for n in 0u16..(RAM_BANK_SIZE as u16) {
                assert_eq!(c.read(0xA000 + n), b + 1);
            }
        }

        // Masking
        c.write(0x4000, 0x80);
        assert_eq!(c.read(0xA000 as u16), 1);
    }

    #[test]
    fn rtc_ignored() {
        let mut c = Mbc3::new(&[], &[]);

        for b in 0x08..=0x0C {
            c.write(0x4000, b);

            for n in 0u16..(RAM_BANK_SIZE as u16) {
                c.write(0xA000 + n, 0xFF);
                assert_eq!(c.read(0xA000 + n), 0x00);
            }
        }
        for b in 0u8..(RAM_BANK_COUNT as u8) {
            c.write(0x4000, b);
            for n in 0u16..(RAM_BANK_SIZE as u16) {
                assert_eq!(c.read(0xA000 + n), 0);
            }
        }
    }
}
