use super::cartridge::Cartridge;
use crate::gameboy::bus::bus::BusMember;

const ROM_BANK_SIZE: usize = 16 * 1024;
const ROM_BANK_COUNT: usize = ROM_BANKS_MAX + 1;
const ROM_BANKS_MAX: usize = 0x1FF;

const RAM_BANK_SIZE: usize = 8 * 1024;
const RAM_BANK_COUNT: usize = RAM_BANKS_MAX + 1;
const RAM_BANKS_MAX: usize = 0x0F;

pub struct Mbc5 {
    rom: Vec<u8>,
    rom_banksel: u16,
    ram: Vec<u8>,
    ram_banksel: u8,
    rom_banks: usize,
}

impl Mbc5 {
    pub fn new(rom: &[u8], save: &[u8]) -> Self {
        let mut cart = Self {
            rom: vec![0; ROM_BANK_COUNT * ROM_BANK_SIZE],
            ram: vec![0; RAM_BANK_COUNT * RAM_BANK_SIZE],
            rom_banksel: 1,
            ram_banksel: 0,
            rom_banks: 0,
        };
        cart.rom[0..rom.len()].copy_from_slice(rom);
        cart.ram[0..save.len()].copy_from_slice(save);

        // Keep this calculated in RAM because it gets looked up a lot.
        cart.rom_banks = cart.get_rom_banks();
        cart
    }

    fn rom_translate(&self, addr: u16) -> usize {
        assert!(addr >= 0x4000);

        let bankaddr: usize =
            ROM_BANK_SIZE * (self.rom_banksel as usize).rem_euclid(self.rom_banks);
        bankaddr + (addr as usize - 0x4000)
    }

    fn ram_translate(&self, addr: u16) -> usize {
        assert!(addr >= 0xA000);

        let bankaddr: usize = RAM_BANK_SIZE * (self.ram_banksel as usize);
        bankaddr + (addr as usize - 0xA000)
    }
}

impl Cartridge for Mbc5 {
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

impl BusMember for Mbc5 {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // ROM - Always bank 0
            0x0000..=0x3FFF => self.rom[addr as usize],
            // ROM - Bank 1..=511
            0x4000..=0x7FFF => self.rom[self.rom_translate(addr)],
            // RAM - Bank 0..=15
            0xA000..=0xBFFF => self.ram[self.ram_translate(addr)],

            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // RAM enable
            0x0000..=0x1FFF => (),
            // ROM bank select (lower 8-bits)
            0x2000..=0x2FFF => self.rom_banksel = val as u16 | (self.rom_banksel & 0x100),
            // ROM bank select (bit 9)
            0x3000..=0x3FFF => self.rom_banksel = (self.rom_banksel & 0xFF) | (val as u16 & 1) << 8,
            // RAM bank select
            0x4000..=0x5FFF => self.ram_banksel = val & RAM_BANKS_MAX as u8,
            // RAM
            0xA000..=0xBFFF => {
                let tr_addr = self.ram_translate(addr);
                self.ram[tr_addr] = val
            }

            _ => (), //panic!("write to {:04X}", addr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::cartridge::*;
    use super::*;

    use itertools::repeat_n;

    #[test]
    fn rom_bank_switching() {
        let mut rom: Vec<u8> = (0u16..=(ROM_BANKS_MAX as u16))
            .flat_map(|i| repeat_n(i.to_le_bytes().into_iter(), ROM_BANK_SIZE / 2).flat_map(|n| n))
            .collect();
        assert_eq!(rom.len(), ROM_BANK_COUNT * ROM_BANK_SIZE);
        rom[CARTTYPE_OFFSET] = CartridgeType::Mbc5 as u8;
        rom[ROMSIZE_OFFSET] = 8; // 8 MB ROM

        let mut c = Mbc5::new(&rom, &[]);

        // Bank 0
        for i in 0u16..(ROM_BANK_SIZE as u16) {
            if i == CARTTYPE_OFFSET as u16 || i == ROMSIZE_OFFSET as u16 {
                assert_ne!(c.read(i), 0);
                continue;
            }
            assert_eq!(c.read(i), 0);
        }
        // Bank n default (1)
        for i in (0..ROM_BANK_SIZE).step_by(2) {
            assert_eq!(c.read16(0x4000 + i as u16), 1);
        }

        // Test each bank n
        for b in 1u16..=(ROM_BANKS_MAX as u16) {
            c.write(0x2000, b as u8);
            c.write(0x3000, ((b & 0x100) >> 8) as u8);
            // Bank 0
            for i in 0..(ROM_BANK_SIZE as u16) {
                if i == CARTTYPE_OFFSET as u16 || i == ROMSIZE_OFFSET as u16 {
                    assert_ne!(c.read(i), 0);
                    continue;
                }
                assert_eq!(c.read(i), 0);
            }
            // Bank n
            for i in (0..ROM_BANK_SIZE).step_by(2) {
                assert_eq!(c.read16(0x4000 + i as u16), b);
            }
        }

        // Test masking
        c.write(0x2000, 0x02);
        c.write(0x3000, 0x02);
        for i in (0..ROM_BANK_SIZE).step_by(2) {
            assert_eq!(c.read16(0x4000 + i as u16), 0x02);
        }

        // Selecting bank 0 should actually select bank 0 on MBC5
        c.write(0x2000, 0);
        c.write(0x3000, 0);
        for i in (CARTHEADER_END..ROM_BANK_SIZE).step_by(2) {
            assert_eq!(c.read16(0x4000 + i as u16), 0x00);
        }
    }

    #[test]
    fn ram_bank_switching() {
        let mut c = Mbc5::new(&[], &[]);

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
}
