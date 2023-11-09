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
                0x2105 => None,
                // BGxSC - BGx Screen Base and Screen Size
                0x2107..=0x210A => Some(self.bgxsc[addr - 0x2107]),
                // BG12NBA/BG34NBA - BG Character Data Area Designation
                0x210B..=0x210C => None,
                // BGxHOFS/BGxVOFS - BGx Horizontal/Vertical Scroll
                0x210D..=0x2114 => None,
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
                // TM - Main Screen Designation
                0x212C => None,
                // TS - Sub Screen Designation
                0x212D => None,
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
                // INIDISP - Display Control 1 (W)
                0x2100 => Some(self.inidisp = val),
                // OBSEL - Object Size and Object Base
                0x2101 => Some(self.obsel = val),
                // OAMADDL - OAM Address and Priority Rotation (W)
                0x2102 => {
                    let v = self.oamadd.get() & 0xFF00;
                    Some(self.oamadd.set(v | val as u16))
                }
                // OAMADDH - OAM Address and Priority Rotation (W)
                0x2103 => {
                    let v = self.oamadd.get() & 0x00FF;
                    let val = val & 0x83; // bit 10-14 unused
                    if val & 0x80 != 0 {
                        // Obj priority
                        // TODO
                    }
                    Some(self.oamadd.set(v | (val as u16) << 8))
                }
                // OAMDATA - OAM Data Write (W)
                0x2104 => {
                    let oaddr = self.oamadd.get();

                    // Deal with the upper table mirrors
                    let addr = if oaddr >= 0x200 {
                        (oaddr & 0x21F) as usize
                    } else {
                        oaddr as usize
                    };

                    let even = addr % 2 == 0;
                    if even {
                        self.oam_writebuf = val;
                    }
                    if !even && addr <= 0x1FF {
                        self.oam[addr - 1] = self.oam_writebuf;
                        self.oam[addr] = val;
                    }
                    if addr > 0x1FF {
                        self.oam[addr] = val;
                    }
                    Some(self.oamadd.set((oaddr + 1) & 0x3FF))
                }
                // BGMODE - BG Mode and BG Character Size
                0x2105 => {
                    if self.bgmode & 7 != val & 7 {
                        println!("PPU screen mode: {}", val & 7);
                    }
                    Some(self.bgmode = val)
                }
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
                // BGxHOFS - BGx Horizontal Scroll (X) (write-twice)
                0x210D | 0x210F | 0x2111 | 0x2113 => {
                    let idx = (addr - 0x210D) / 2;
                    let cur = self.bgxhofs[idx];
                    let prev = self.bgxxofs_prev as u16;
                    let new = val as u16;
                    self.bgxxofs_prev = val;

                    Some(self.bgxhofs[idx] = (new << 8) | (prev & !7) | ((cur >> 8) & 7))
                }
                // BGxVOFS - BGx Vertical Scroll (Y) (write-twice)
                0x210E | 0x2110 | 0x2112 | 0x2114 => {
                    let idx = (addr - 0x210E) / 2;
                    let prev = self.bgxxofs_prev as u16;
                    let new = val as u16;
                    self.bgxxofs_prev = val;

                    Some(self.bgxvofs[idx] = (new << 8) | prev)
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
                // TM - Main Screen Designation
                0x212C => Some(self.tm = val),
                // TS - Sub Screen Designation
                0x212D => Some(self.ts = val),
                // RDCGRAM - Palette CGRAM Data Read
                0x213B => None,

                _ => None,
            },

            _ => None,
        }
    }
}
