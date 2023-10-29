pub mod bus;
pub mod render;

#[cfg(test)]
pub mod tests;

use std::cell::Cell;

use anyhow::Result;
use num_derive::ToPrimitive;
use num_traits::ToPrimitive;

use crate::frontend::Renderer;
use crate::tickable::{Tickable, Ticks};

pub const SCREEN_WIDTH: usize = 8 * 32;
pub const SCREEN_HEIGHT: usize = 8 * 32;

type VramWord = u16;
const VRAM_WORDS: usize = 32 * 1024;
const VRAM_WORDSIZE: usize = 2;
// 32K-words addressable (64KB)
const VRAM_ADDRMASK: usize = 0x7FFF;

type CgramWord = u16;
const CGRAM_WORDS: usize = 256;
const CGRAM_WORDSIZE: usize = 2;
const CGRAM_ADDRMASK: usize = 0xFF;

// VMAIN bits
const VMAIN_HIGH: u8 = 1 << 7;
const VMAIN_INC_MASK: u8 = 0x03;
const VMAIN_TRANSLATE_MASK: u8 = 0x03;
const VMAIN_TRANSLATE_SHIFT: u8 = 2;

pub struct TilemapEntry(u16);
impl TilemapEntry {
    fn charnr(&self) -> u16 {
        self.0 & 0x1FF
    }

    fn palettenr(&self) -> u8 {
        ((self.0 >> 10) & 0x07) as u8
    }

    fn bgprio(&self) -> bool {
        (self.0 & (1 << 13)) != 0
    }

    fn flip_x(&self) -> bool {
        (self.0 & (1 << 14)) != 0
    }

    fn flip_y(&self) -> bool {
        (self.0 & (1 << 15)) != 0
    }
}

#[derive(ToPrimitive)]
pub enum BPP {
    // BPP == number of bitplanes
    Two = 2,   // 4 colors
    Four = 4,  // 16 colors
    Eight = 8, // 256 colors
}

pub struct PPU<TRenderer: Renderer> {
    renderer: TRenderer,
    cycles: usize,
    last_scanline: usize,
    intreq_vblank: bool,
    vblank: bool,

    vram: Vec<VramWord>,
    vmadd: Cell<u16>,
    vmain: u8,

    /// Palette RAM (CGRAM)
    cgram: Vec<CgramWord>,

    /// CGADD register
    cgadd: Cell<u8>,

    /// MSB/LSB read/write flip-flop for CGRAM
    cgadd_msb: Cell<bool>,

    bgmode: u8,
    bgxsc: [u8; 4],
    bgxnba: [u8; 4],
}

pub struct Tile<'a> {
    data: &'a [u16],
    bpp: BPP,
}
impl<'a> Tile<'a> {
    pub fn get_coloridx(&self, x: usize, y: usize) -> u8 {
        let mut result = 0;
        let bitplanes = self.bpp.to_usize().unwrap();
        let bitp_w = bitplanes / VRAM_WORDSIZE;

        for i in 0..bitp_w {
            if self.data[(y * bitp_w) + i] & (1 << 7 - x) != 0 {
                result |= 1 << i;
            }
            if self.data[(y * bitp_w) + i] & (1 << 15 - x) != 0 {
                result |= 1 << (i + 1);
            }
        }
        result
    }
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    const CYCLES_PER_SCANLINE: usize = 1364;
    const SCANLINES_PER_FRAME: usize = 262;
    const VBLANK_START: usize = 0xE1;

    pub fn new(renderer: TRenderer) -> Self {
        Self {
            renderer,
            cycles: 0,
            last_scanline: 0,
            intreq_vblank: false,
            vblank: false,

            vram: vec![0; VRAM_WORDS],
            vmadd: Cell::new(0),
            vmain: 0,

            cgram: vec![0; CGRAM_WORDS],
            cgadd: Cell::new(0),
            cgadd_msb: Cell::new(false),

            bgmode: 0,
            bgxsc: [0, 0, 0, 0],
            bgxnba: [0, 0, 0, 0],
        }
    }

    fn vram_autoinc(&self, upper: bool) {
        let inc_on_upper = self.vmain & VMAIN_HIGH != 0;
        if upper != inc_on_upper {
            return;
        }

        // TODO prefetch BEFORE increment
        let inc = match self.vmain & VMAIN_INC_MASK {
            0 => 1,
            1 => 32,
            2..=3 => 128,
            _ => unreachable!(),
        };
        self.vmadd.set(self.vmadd.get().wrapping_add(inc));
    }

    fn vram_addr_translate(&self, addr: u16) -> u16 {
        // Translation  Bitmap Type              Port [2116h/17h]    VRAM Word-Address
        //  8bit rotate  4-color; 1 word/plane    aaaaaaaaYYYxxxxx --> aaaaaaaaxxxxxYYY
        //  9bit rotate  16-color; 2 words/plane  aaaaaaaYYYxxxxxP --> aaaaaaaxxxxxPYYY
        // 10bit rotate 256-color; 4 words/plane aaaaaaYYYxxxxxPP --> aaaaaaxxxxxPPYYY
        match (self.vmain & VMAIN_TRANSLATE_MASK) >> VMAIN_TRANSLATE_SHIFT {
            0 => addr,
            1..=3 => todo!(),
            _ => unreachable!(),
        }
    }

    fn get_bg_map_addr(&self, bg: usize) -> usize {
        assert!((0..4).contains(&bg));
        (self.bgxsc[bg] >> 2) as usize * 1024
    }

    fn get_tilemap_offset(&self, bg: usize, tileidx: usize) -> usize {
        // Each map entry is one word.
        let map_addr = self.get_bg_map_addr(bg);
        (map_addr + tileidx) & VRAM_ADDRMASK
    }

    fn get_tilemap_entry(&self, bg: usize, tileidx: usize) -> TilemapEntry {
        TilemapEntry(self.vram[self.get_tilemap_offset(bg, tileidx)])
    }

    fn get_screen_mode(&self) -> u8 {
        self.bgmode & 0x07
    }

    fn get_tilemap_entry_xy(&self, bg: usize, x: usize, y: usize) -> TilemapEntry {
        // TODO scrolling

        match self.get_screen_mode() {
            0 => {
                if !self.is_layer_16x16(bg) {
                    let tm_x = x / 8;
                    let tm_y = y.rem_euclid(32 * 8) / 8;
                    self.get_tilemap_entry(bg, tm_x + (tm_y * 32))
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn get_layer_bpp(&self, bg: usize) -> BPP {
        match bg {
            0 => BPP::Two,
            1 => match bg {
                0 => BPP::Four,
                1 => BPP::Four,
                2 => BPP::Two,
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn get_tile(&self, bg: usize, tile: &TilemapEntry) -> Tile {
        let idx = (self.bgxnba[bg] as usize * 4096) + (tile.charnr() as usize * 8);
        Tile {
            data: &self.vram[idx..(idx + 8)],
            bpp: self.get_layer_bpp(bg),
        }
    }

    fn is_layer_16x16(&self, bg: usize) -> bool {
        assert!((0..4).contains(&bg));
        self.bgmode & (1 << (7 - bg)) != 0
    }

    fn get_current_scanline(&self) -> usize {
        self.cycles / Self::CYCLES_PER_SCANLINE
    }

    pub fn in_vblank(&self) -> bool {
        self.last_scanline >= Self::VBLANK_START
    }
}

impl<TRenderer> Tickable for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        self.cycles =
            (self.cycles + ticks) % (Self::CYCLES_PER_SCANLINE * Self::SCANLINES_PER_FRAME);

        if self.get_current_scanline() != self.last_scanline {
            self.last_scanline = self.get_current_scanline();

            if !self.vblank && self.in_vblank() {
                // Entered VBlank
                self.vblank = true;
                self.intreq_vblank = true;
                self.renderer.update()?;
            } else {
                self.vblank = false;
                if self.last_scanline < SCREEN_HEIGHT {
                    self.render_scanline(self.last_scanline);
                }
            }
        }

        Ok(())
    }
}
