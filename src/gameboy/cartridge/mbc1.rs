use super::cartridge::Cartridge;
use crate::gameboy::bus::bus::BusMember;

use std::cmp;

const ROM_BANK_SIZE: usize = 16 * 1024;
const ROM_BANK_COUNT: usize = ROM_BANKS_MAX + 1;
const ROM_BANKS_MAX: usize = 0x7F;

const RAM_BANK_SIZE: usize = 8 * 1024;
const RAM_BANK_COUNT: usize = RAM_BANKS_MAX + 1;
const RAM_BANKS_MAX: usize = 0x03;

pub struct Mbc1 {
    rom: Vec<u8>,
    bank1: u8,
    ram: Vec<u8>,
    bank2: u8,

    ram_enable: bool,
    bank_advanced: bool,
    rom_banks: usize,
    ram_banks: usize,
}

impl Mbc1 {
    pub fn new(rom: &[u8], save: &[u8]) -> Self {
        let mut cart = Self {
            rom: vec![0; ROM_BANK_COUNT * ROM_BANK_SIZE],
            ram: vec![0; RAM_BANK_COUNT * RAM_BANK_SIZE],
            bank1: 1,
            bank2: 0,
            ram_enable: false,
            bank_advanced: false,
            rom_banks: 0,
            ram_banks: 0,
        };
        cart.rom[0..rom.len()].copy_from_slice(rom);
        cart.rom_banks = cart.get_rom_banks();
        cart.ram_banks = cart.get_ram_banks();
        cart.ram[0..save.len()].copy_from_slice(save);
        cart
    }

    fn rom_translate_0(&self, addr: u16) -> usize {
        assert!(addr < 0x4000);
        let bank = if self.bank_advanced {
            (self.bank2 << 5) & (self.rom_banks - 1) as u8
        } else {
            0
        } as usize;
        let bankaddr: usize = ROM_BANK_SIZE * bank;

        bankaddr + (addr as usize)
    }

    fn rom_translate_1(&self, addr: u16) -> usize {
        assert!(addr >= 0x4000);

        let bankmask = (self.rom_banks - 1) as usize;
        let bank = ((cmp::max(1, self.bank1) | self.bank2 << 5) as usize) & bankmask;
        let bankaddr: usize = ROM_BANK_SIZE * bank;

        bankaddr + (addr as usize - 0x4000)
    }

    fn ram_translate(&self, addr: u16) -> usize {
        assert!(addr >= 0xA000);

        let bankaddr: usize = if self.bank_advanced && self.ram_banks > 0 {
            RAM_BANK_SIZE * ((self.bank2 as usize) & (self.ram_banks - 1))
        } else {
            // Always bank 0 in mode 0
            0
        };
        bankaddr + (addr as usize - 0xA000)
    }
}

impl Cartridge for Mbc1 {
    fn dump_state(&self) -> String {
        format!(
            "ROM: {:02X} - RAM : {:02X} - Mode: {}",
            self.bank1,
            self.bank2,
            if self.bank_advanced { 1 } else { 0 }
        )
    }

    fn get_save(&self) -> Vec<u8> {
        self.ram.to_owned()
    }
}

impl BusMember for Mbc1 {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // ROM - Always bank 0
            0x0000..=0x3FFF => self.rom[self.rom_translate_0(addr)],
            // ROM - Bank 1..n
            0x4000..=0x7FFF => self.rom[self.rom_translate_1(addr)],
            // RAM - Bank 0..=3
            0xA000..=0xBFFF => {
                if self.ram_enable {
                    self.ram[self.ram_translate(addr)]
                } else {
                    0xFF
                }
            }

            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // RAM enable
            0x0000..=0x1FFF => self.ram_enable = val & 0x0F == 0x0A,
            // Bank register 1
            0x2000..=0x3FFF => self.bank1 = val & 0x1F,
            // Bank register 2
            0x4000..=0x5FFF => self.bank2 = val & 0x03,
            // Banking mode select
            0x6000..=0x7FFF => self.bank_advanced = val & 1 == 1,
            // RAM - Bank 0..=3
            0xA000..=0xBFFF => {
                if self.ram_enable {
                    let tr_addr = self.ram_translate(addr);
                    self.ram[tr_addr] = val;
                }
            }

            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::cartridge::*;
    use super::*;

    use itertools::repeat_n;

    #[test]
    fn rom_bank_switching_mode0() {
        let mut rom: Vec<u8> = (0u8..=(ROM_BANKS_MAX as u8))
            .flat_map(|i| repeat_n(i, ROM_BANK_SIZE))
            .collect();
        assert_eq!(rom.len(), ROM_BANK_COUNT * ROM_BANK_SIZE);
        rom[CARTTYPE_OFFSET] = CartridgeType::Mbc1 as u8;
        rom[ROMSIZE_OFFSET] = 0x06; // 2MB ROM

        let mut c = Mbc1::new(&rom, &[]);

        // Bank 0
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            if i == CARTTYPE_OFFSET as u16 || i == ROMSIZE_OFFSET as u16 {
                assert_ne!(c.read(i), 0);
                continue;
            }
            assert_eq!(c.read(i), 0);
        }
        // Bank n default (1)
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 1);
        }

        // Test each bank n
        for b in 1u8..128 {
            c.write(0x2000, b & 0x1F);
            c.write(0x4000, b >> 5);

            // Bank 0
            for i in 0..(ROM_BANK_SIZE as u16) {
                if i == CARTTYPE_OFFSET as u16 || i == ROMSIZE_OFFSET as u16 {
                    assert_ne!(c.read(i), 0);
                    continue;
                }
                assert_eq!(c.read(i), 0);
            }
            // Bank n (with addressing quirk)
            let expected = cmp::max(b & 0x1F, 1) | (b & !0x1F);
            for i in 0u16..(ROM_BANK_SIZE as u16) {
                assert_eq!(c.read(0x4000 + i), expected);
            }
        }

        // Test masking
        c.write(0x2000, 0x82);
        c.write(0x4000, 0);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x02);
        }

        // Selecting bank 0 should select bank 1
        c.write(0x2000, 0);
        c.write(0x4000, 0);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x01);
        }

        // Selecting bank 0x20 should select bank 0x61 (quirk)
        c.write(0x2000, 0x00);
        c.write(0x4000, 0x01);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x21);
        }
    }

    #[test]
    fn rom_bank_switching_mode1() {
        let mut rom: Vec<u8> = (0u8..=(ROM_BANKS_MAX as u8))
            .flat_map(|i| repeat_n(i, ROM_BANK_SIZE))
            .collect();
        assert_eq!(rom.len(), ROM_BANK_COUNT * ROM_BANK_SIZE);
        rom[CARTTYPE_OFFSET] = CartridgeType::Mbc1 as u8;
        rom[ROMSIZE_OFFSET] = 0x06; // 2MB ROM

        let mut c = Mbc1::new(&rom, &[]);

        // Select mode 1
        c.write(0x6000, 1);

        // Bank 0
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            if i == CARTTYPE_OFFSET as u16 || i == ROMSIZE_OFFSET as u16 {
                assert_ne!(c.read(i), 0);
                continue;
            }
            assert_eq!(c.read(i), 0);
        }

        // Test each bank n
        for b in 1u8..128 {
            c.write(0x2000, b & 0x1F);
            c.write(0x4000, b >> 5);

            // Bank n
            for i in 0..(ROM_BANK_SIZE as u16) {
                if i == CARTTYPE_OFFSET as u16 || i == ROMSIZE_OFFSET as u16 {
                    continue;
                }
                assert_eq!(c.read(i), b & !0x1F);
            }
        }

        // Test masking, 0x4000 should function the same
        c.write(0x2000, 0x82);
        c.write(0x4000, 0);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x02);
        }

        // Selecting bank 0 should select bank 1,
        // should still be the same for 0x4000
        c.write(0x2000, 0);
        c.write(0x4000, 0);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x01);
        }

        // Selecting bank 0x20 should select bank 0x61 (quirk)
        // at 0x4000, but not at 0x0000
        c.write(0x2000, 0x00);
        c.write(0x4000, 0x01);
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            assert_eq!(c.read(0x4000 + i), 0x21);
            assert_eq!(c.read(0x0000 + i), 0x20);
        }
    }

    #[test]
    fn ram_bank_switching() {
        let mut rom: [u8; CARTHEADER_END] = [0; CARTHEADER_END];
        rom[CARTTYPE_OFFSET] = CartridgeType::Mbc1Ram as u8;
        rom[RAMSIZE_OFFSET] = 0x03; // 32kb RAM
        let mut c = Mbc1::new(&rom, &[]);

        c.write(0x0000, 0x0A); // RAM enable
        c.write(0x6000, 1); // Banking mode 1

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
        c.write(0x4000, RAM_BANK_COUNT as u8);
        assert_eq!(c.read(0xA000 as u16), 1);
    }

    #[test]
    fn ram_enable() {
        let mut rom: [u8; CARTHEADER_END] = [0; CARTHEADER_END];
        rom[CARTTYPE_OFFSET] = CartridgeType::Mbc1Ram as u8;
        rom[RAMSIZE_OFFSET] = 0x03; // 32kb RAM
        let mut c = Mbc1::new(&rom, &[]);

        assert_eq!(c.read(0xA000), 0xFF);
        c.write(0xA000, 0xAB);
        assert_eq!(c.read(0xA000), 0xFF);

        c.write(0x0000, 0x0A); // RAM enable
                               // Earlier write should not have had any effect.
        assert_eq!(c.read(0xA000), 0x00);
        c.write(0xA000, 0xAB);
        assert_eq!(c.read(0xA000), 0xAB);

        // Disabling and enabling should reveal the original
        // data and writing while disabled shouldn't overwrite it.
        c.write(0x0000, 0x00); // RAM disable
        assert_eq!(c.read(0xA000), 0xFF);
        c.write(0xA000, 0xCD);
        c.write(0x0000, 0x0A); // RAM enable
        assert_eq!(c.read(0xA000), 0xAB);
    }
}
