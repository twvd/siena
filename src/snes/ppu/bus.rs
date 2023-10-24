use super::{PPU, VRAM_ADDRMASK};

use crate::snes::bus::{Address, BusMember};

impl BusMember for PPU {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                // BGMODE - BG Mode and BG Character Size
                0x2015 => Some(self.bgmode),
                // BGxSC - BGx Screen Base and Screen Size
                0x2107..=0x210A => Some(self.bgxsc[addr - 0x2107]),
                // VMAIN - VRAM Address Increment Mode
                0x2115 => Some(self.vmain),
                // VMADDL - VRAM Address (lower 8bit)
                0x2116 => Some(self.vmadd.get() as u8),
                // VMADDH - VRAM Address (upper 8bit)
                0x2117 => Some((self.vmadd.get() >> 8) as u8),
                // VMDATAL - VRAM Data write (lower 8bit)
                0x2118 => None,
                // VMDATAH - VRAM Data write (upper 8bit)
                0x2119 => None,
                // RDVRAML - VRAM Data Read (lower 8bit)
                // TODO prefetch glitch
                //0x2139 => {
                //    let v = Some(self.vram[self.vmadd.get() as usize & VRAM_ADDRMASK] as u8);
                //    self.vram_autoinc(false);
                //    v
                //}
                // RDVRAMH - VRAM Data Read (upper 8bit)
                // TODO prefetch glitch
                //0x213A => {
                //    let v = Some((self.vram[self.vmadd.get() as usize & VRAM_ADDRMASK] >> 8) as u8);
                //    self.vram_autoinc(true);
                //    v
                //}
                _ => None,
            },
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                // BGMODE - BG Mode and BG Character Size
                0x2015 => Some(self.bgmode = val),
                // BGxSC - BGx Screen Base and Screen Size
                0x2107..=0x210A => Some(self.bgxsc[addr - 0x2107] = val),
                // VMAIN - VRAM Address Increment Mode
                0x2115 => Some(self.vmain = val),
                // VMADDL - VRAM Address (lower 8bit)
                0x2116 => {
                    let v = self.vmadd.get() & 0xFF00;
                    Some(self.vmadd.set(v | val as u16))
                }
                // VMADDH - VRAM Address (upper 8bit)
                0x2117 => {
                    let v = self.vmadd.get() & 0x00FF;
                    Some(self.vmadd.set(v | (val as u16) << 8))
                }
                // VMDATAL - VRAM Data write (lower 8bit)
                0x2118 => {
                    let addr =
                        usize::from(self.vram_addr_translate(self.vmadd.get())) & VRAM_ADDRMASK;
                    let cur = self.vram[addr];

                    self.vram_autoinc(false);
                    Some(self.vram[addr] = (cur & 0xFF00) | val as u16)
                }
                // VMDATAH - VRAM Data write (upper 8bit)
                0x2119 => {
                    let addr =
                        usize::from(self.vram_addr_translate(self.vmadd.get())) & VRAM_ADDRMASK;
                    let cur = self.vram[addr];

                    self.vram_autoinc(true);
                    Some(self.vram[addr] = (cur & 0xFF) | (val as u16) << 8)
                }

                _ => None,
            },

            _ => None,
        }
    }
}
