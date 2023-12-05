pub mod bus;
pub mod color;
pub mod render;
pub mod sprites;
pub mod tile;

#[cfg(test)]
pub mod tests;

use std::cell::Cell;

use anyhow::Result;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use serbia::serbia;
use serde::{Deserialize, Serialize};

use crate::frontend::Renderer;
use crate::tickable::{Tickable, Ticks};

use color::SnesColor;
use tile::{Tile, TILE_HEIGHT, TILE_WIDTH};

pub const SCREEN_WIDTH: usize = 8 * 32;
pub const SCREEN_HEIGHT: usize = 8 * 28;

// The entire screen should be shifted up by 1 scanline
const SCANLINE_OUTPUT_OFFSET: isize = -1;

type VramWord = u16;
const VRAM_WORDS: usize = 32 * 1024;
const VRAM_WORDSIZE: usize = 2;
// 32K-words addressable (64KB)
const VRAM_ADDRMASK: usize = VRAM_WORDS - 1;

type CgramWord = u16;
const CGRAM_WORDS: usize = 256;
#[allow(dead_code)]
const CGRAM_WORDSIZE: usize = 2;
#[allow(dead_code)]
const CGRAM_ADDRMASK: usize = CGRAM_WORDS - 1;

// VMAIN bits
const VMAIN_HIGH: u8 = 1 << 7;
const VMAIN_INC_MASK: u8 = 0x03;
const VMAIN_TRANSLATE_MASK: u8 = 0x03;
const VMAIN_TRANSLATE_SHIFT: u8 = 2;

const OAM_SIZE: usize = 512 + 32;

#[derive(FromPrimitive)]
pub enum TilemapDimensions {
    D32x32 = 0,
    D64x32 = 1,
    D32x64 = 2,
    D64x64 = 3,
}

#[derive(Debug)]
pub struct TilemapEntry(u16);
impl TilemapEntry {
    fn charnr(&self) -> u16 {
        self.0 & 0x3FF
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

#[derive(Clone, Copy, Debug, ToPrimitive)]
pub enum BPP {
    // BPP == number of bitplanes
    Two = 2,   // 4 colors
    Four = 4,  // 16 colors
    Eight = 8, // 256 colors
}
impl BPP {
    pub fn entries_per_palette(&self) -> u8 {
        1 << self.to_u8().unwrap()
    }
    pub fn num_bitplanes(&self) -> usize {
        self.to_usize().unwrap()
    }
}

fn _default_none<T>() -> Option<T> {
    None
}

#[serbia]
#[derive(Serialize, Deserialize)]
pub struct PPU<TRenderer: Renderer> {
    // Debug toggles to mask certain bg/obj layers
    pub dbg_layermask: u8,

    #[serde(skip, default = "_default_none")]
    pub renderer: Option<TRenderer>,

    cycles: usize,
    last_scanline: usize,
    intreq_vblank: bool,
    intreq_hblank: bool,
    vblank: bool,
    hblank: bool,

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
    bgxhofs: [u16; 4],
    bgxvofs: [u16; 4],
    bgxxofs_prev: u8,

    tm: u8,
    ts: u8,

    obsel: u8,
    oamadd: Cell<u16>,
    oam: [u8; OAM_SIZE],
    oam_writebuf: u8,

    inidisp: u8,

    // Window settings
    w1_left: u8,
    w1_right: u8,
    w2_left: u8,
    w2_right: u8,
    tmw: u8,
    tsw: u8,
    w12sel: u8,
    w34sel: u8,
    wobjsel: u8,
    wbglog: u8,
    wobjlog: u8,

    // Color math settings
    cgwsel: u8,
    cgadsub: u8,
    coldata: SnesColor,

    // H/V latches
    hlatch: Cell<u8>,
    vlatch: Cell<u8>,

    m7a: u16,
    m7b: u8,
}

pub struct BgTile<'a> {
    data: &'a [u16],
    map: &'a TilemapEntry,
    bpp: BPP,
}
impl<'a, 'tdata> Tile<'tdata> for BgTile<'a>
where
    'a: 'tdata,
{
    fn get_tile_data(&self) -> &'tdata [VramWord] {
        self.data
    }
    fn get_tile_flip_x(&self) -> bool {
        self.map.flip_x()
    }
    fn get_tile_flip_y(&self) -> bool {
        self.map.flip_y()
    }
    fn get_tile_bpp(&self) -> BPP {
        self.bpp
    }
    fn get_tile_palette(&self) -> u8 {
        self.map.palettenr()
    }
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    const CYCLES_PER_SCANLINE: usize = 340 * 4;
    const SCANLINES_PER_FRAME: usize = 262; // including V-blank
    const VBLANK_START: usize = 0xE1;
    const LINE_HBLANK_START: usize = 274 * 4;

    pub fn new(renderer: TRenderer) -> Self {
        Self {
            dbg_layermask: 0,

            renderer: Some(renderer),
            cycles: 0,
            last_scanline: 0,
            intreq_vblank: false,
            intreq_hblank: false,
            vblank: false,
            hblank: false,

            vram: vec![0; VRAM_WORDS],
            vmadd: Cell::new(0),
            vmain: 0,

            cgram: vec![0; CGRAM_WORDS],
            cgadd: Cell::new(0),
            cgadd_msb: Cell::new(false),

            bgmode: 0,
            bgxsc: [0, 0, 0, 0],
            bgxnba: [0, 0, 0, 0],
            bgxhofs: [0, 0, 0, 0],
            bgxvofs: [0, 0, 0, 0],
            bgxxofs_prev: 0,
            tm: 0,
            ts: 0,

            obsel: 0,
            oamadd: Cell::new(0),
            oam: [0; OAM_SIZE],
            oam_writebuf: 0,

            inidisp: 0,

            w1_left: 0,
            w1_right: 0,
            w2_left: 0,
            w2_right: 0,
            tmw: 0,
            tsw: 0,
            w12sel: 0,
            w34sel: 0,
            wobjsel: 0,
            wbglog: 0,
            wobjlog: 0,

            cgwsel: 0,
            cgadsub: 0,
            coldata: SnesColor::BLACK,

            hlatch: Cell::new(0),
            vlatch: Cell::new(0),

            m7a: 0,
            m7b: 0,
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
        debug_assert!((0..4).contains(&bg));
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

    fn get_tilemap_dimensions(&self, bg: usize) -> TilemapDimensions {
        TilemapDimensions::from_u8(self.bgxsc[bg] & 0x03).unwrap()
    }

    fn get_tilemap_entry_xy(&self, bg: usize, x: usize, y: usize) -> TilemapEntry {
        let bghofs = self.bgxhofs[bg] as usize;
        let bgvofs = self.bgxvofs[bg] as usize;
        let tilesize = self.get_bg_tile_size(bg);

        match self.get_screen_mode() {
            0 | 1 | 2 | 3 => {
                // AA BB CC DD, size = 0x800 per sub-map
                // 00  32x32   AA
                //             AA
                // 01  64x32   AB
                //             AB
                // 10  32x64   AA
                //             BB
                // 11  64x64   AB
                //             CD
                let (expand_x, expand_y) = match self.get_tilemap_dimensions(bg) {
                    TilemapDimensions::D32x32 => (false, false),
                    TilemapDimensions::D32x64 => (false, true),
                    TilemapDimensions::D64x32 => (true, false),
                    TilemapDimensions::D64x64 => (true, true),
                };
                let tm_x = (bghofs + x) / tilesize;
                let tm_y = (bgvofs + y) / tilesize;

                // 32 tiles per row in the sub-map, 0-31
                let mut idx = ((tm_y & 0x1F) << 5) + (tm_x & 0x1F);
                if expand_y {
                    if expand_x {
                        idx += (tm_y & 0x20) << 6;
                    } else {
                        idx += (tm_y & 0x20) << 5;
                    }
                }
                if expand_x {
                    idx += (tm_x & 0x20) << 5;
                }

                self.get_tilemap_entry(bg, idx)
            }
            _ => todo!(),
        }
    }

    fn get_layer_bpp(&self, bg: usize) -> BPP {
        match self.get_screen_mode() {
            0 => BPP::Two,
            1 => match bg {
                0 => BPP::Four,
                1 => BPP::Four,
                2 => BPP::Two,
                _ => unreachable!(),
            },
            2 => match bg {
                0 => BPP::Four,
                1 => BPP::Four,
                _ => unreachable!(),
            },
            3 => match bg {
                0 => BPP::Eight,
                1 => BPP::Four,
                _ => unreachable!(),
            },
            _ => todo!(),
        }
    }

    /// Retrieve a pixel data reference to a specific bg layer tile.
    fn get_bg_tile<'a>(
        &'a self,
        bg: usize,
        entry: &'a TilemapEntry,
        px_x: usize,
        px_y: usize,
    ) -> BgTile {
        let bpp = self.get_layer_bpp(bg);
        let len = TILE_HEIGHT * bpp.num_bitplanes() / VRAM_WORDSIZE;

        // Determine tile/character table index
        // For 8x8, always what is indicated by the map.
        // For 16x16:
        // T+00 T+01
        // T+16 T+17
        let mut tilenr = entry.charnr() as usize;
        if self.get_bg_tile_size(bg) == 16 && px_x > TILE_WIDTH {
            tilenr += 1;
        }
        if self.get_bg_tile_size(bg) == 16 && px_y > TILE_HEIGHT {
            tilenr += 16;
        }

        let idx = ((self.bgxnba[bg] as usize * 4096) + (tilenr * len)) & VRAM_ADDRMASK;
        BgTile {
            data: &self.vram[idx..(idx + len)],
            bpp,
            map: entry,
        }
    }

    pub fn get_bg_tile_size(&self, bg: usize) -> usize {
        debug_assert!((0..4).contains(&bg));
        if self.bgmode & (1 << (4 + bg)) != 0 {
            16
        } else {
            8
        }
    }

    pub fn get_current_scanline(&self) -> usize {
        self.cycles / Self::CYCLES_PER_SCANLINE
    }

    pub fn get_current_h(&self) -> usize {
        self.cycles % Self::CYCLES_PER_SCANLINE
    }

    pub fn in_vblank(&self) -> bool {
        self.last_scanline >= Self::VBLANK_START
    }

    pub fn in_hblank(&self) -> bool {
        self.cycles % Self::CYCLES_PER_SCANLINE >= Self::LINE_HBLANK_START
    }

    pub fn get_clr_intreq_vblank(&mut self) -> bool {
        let v = self.intreq_vblank;
        self.intreq_vblank = false;
        v
    }

    pub fn get_clr_intreq_hblank(&mut self) -> bool {
        let v = self.intreq_hblank;
        self.intreq_hblank = false;
        v
    }
}

impl<TRenderer> Tickable for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        self.cycles =
            (self.cycles + ticks) % (Self::CYCLES_PER_SCANLINE * Self::SCANLINES_PER_FRAME);

        if self.in_hblank() {
            if !self.hblank {
                // Entered HBlank
                self.hblank = true;
                self.intreq_hblank = true;
            }
        } else {
            self.hblank = false;
        }

        if self.get_current_scanline() != self.last_scanline {
            self.last_scanline = self.get_current_scanline();

            if self.in_vblank() {
                if !self.vblank {
                    // Entered VBlank
                    self.vblank = true;
                    self.intreq_vblank = true;

                    let renderer = self.renderer.as_mut().unwrap();
                    renderer.update()?;
                }
            } else {
                self.vblank = false;
                if (self.last_scanline as isize)
                    < ((SCREEN_HEIGHT as isize) - SCANLINE_OUTPUT_OFFSET)
                    && (self.last_scanline as isize) + SCANLINE_OUTPUT_OFFSET >= 0
                {
                    self.render_scanline(self.last_scanline, SCANLINE_OUTPUT_OFFSET);
                }
            }
        }

        Ok(())
    }
}
