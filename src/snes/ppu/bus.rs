use super::*;

use crate::frontend::Renderer;
use crate::snes::bus::{Address, BusMember};

impl<TRenderer> BusMember for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                // BGMODE - BG Mode and BG Character Size
                0x2015 => Some(self.bgmode),
                // BGxSC - BGx Screen Base and Screen Size
                0x2107..=0x210A => Some(self.bgxsc[addr - 0x2107]),
                // BG12NBA/BG34NBA - BG Character Data Area Designation
                0x210B..=0x210C => None,
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
                // CGADD - Palette CGRAM Address (Color Generator Memory)
                0x2121 => None,
                // CGDATA - Palette CGRAM Data Write
                0x2122 => None,
                // RDCGRAM - Palette CGRAM Data Read
                0x213B => {
                    let addr = self.cgadd.get();
                    let msb = self.cgadd_msb.get();
                    let valw = self.cgram[addr as usize];
                    let valb = if msb {
                        (valw >> 8) as u8
                    } else {
                        (valw & 0xFF) as u8
                    };

                    if msb {
                        // Increment address
                        self.cgadd.set(addr.wrapping_add(1));
                        self.cgadd_msb.set(false);
                    } else {
                        self.cgadd_msb.set(true);
                    }

                    Some(valb)
                }

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
                // BG12NBA - BG Character Data Area Designation
                0x210B => {
                    self.bgxnba[0] = val & 0x0F;
                    self.bgxnba[1] = (val >> 4) & 0x0F;
                    Some(())
                }
                // BG34NBA - BG Character Data Area Designation
                0x210C => {
                    self.bgxnba[2] = val & 0x0F;
                    self.bgxnba[3] = (val >> 4) & 0x0F;
                    Some(())
                }
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
                // CGADD - Palette CGRAM Address (Color Generator Memory)
                0x2121 => {
                    self.cgadd.set(val);
                    self.cgadd_msb.set(false);
                    Some(())
                }
                // CGDATA - Palette CGRAM Data Write
                0x2122 => {
                    let addr = self.cgadd.get();
                    let msb = self.cgadd_msb.get();
                    let valw = self.cgram[addr as usize];

                    if msb {
                        self.cgram[addr as usize] = valw & 0xFF | ((val as CgramWord) << 8);

                        self.cgadd.set(addr.wrapping_add(1));
                        self.cgadd_msb.set(false);
                    } else {
                        self.cgram[addr as usize] = valw & 0xFF00 | val as CgramWord;
                        self.cgadd_msb.set(true);
                    }
                    Some(())
                }
                // RDCGRAM - Palette CGRAM Data Read
                0x213B => None,

                _ => None,
            },

            _ => None,
        }
    }
}
