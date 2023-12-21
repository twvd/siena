use std::cell::Cell;

use anyhow::Result;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use serbia::serbia;
use serde::{Deserialize, Serialize};

use super::color::SnesColor;
use super::tile::{Tile, TILE_HEIGHT, TILE_WIDTH};

pub type VramWord = u16;
pub const VRAM_WORDS: usize = 32 * 1024;
pub const VRAM_WORDSIZE: usize = 2;
// 32K-words addressable (64KB)
pub const VRAM_ADDRMASK: usize = VRAM_WORDS - 1;

pub type CgramWord = u16;
pub const CGRAM_WORDS: usize = 256;
#[allow(dead_code)]
pub const CGRAM_WORDSIZE: usize = 2;
#[allow(dead_code)]
pub const CGRAM_ADDRMASK: usize = CGRAM_WORDS - 1;

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
    pub fn charnr(&self) -> u16 {
        self.0 & 0x3FF
    }

    pub fn palettenr(&self) -> u8 {
        ((self.0 >> 10) & 0x07) as u8
    }

    pub fn bgprio(&self) -> bool {
        (self.0 & (1 << 13)) != 0
    }

    pub fn flip_x(&self) -> bool {
        (self.0 & (1 << 14)) != 0
    }

    pub fn flip_y(&self) -> bool {
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

#[serbia]
#[derive(Serialize, Deserialize)]
pub struct PPUState {
    // Debug toggles to mask certain bg/obj layers
    pub dbg_layermask: u8,

    pub(super) vram: Vec<VramWord>,
    pub(super) vmadd: Cell<u16>,
    pub(super) vmain: u8,
    pub(super) vram_prefetch: Cell<u16>,

    /// Palette RAM (CGRAM)
    pub(super) cgram: Vec<CgramWord>,

    /// CGADD register
    pub(super) cgadd: Cell<u8>,

    /// MSB/LSB read/write flip-flop for CGRAM
    pub(super) cgadd_msb: Cell<bool>,

    pub(super) bgmode: u8,
    pub(super) bgxsc: [u8; 4],
    pub(super) bgxnba: [u8; 4],
    pub(super) bgxhofs: [u16; 4],
    pub(super) bgxvofs: [u16; 4],
    pub(super) bgxxofs_prev: u8,

    pub(super) tm: u8,
    pub(super) ts: u8,

    pub(super) obsel: u8,
    pub(super) oamadd_reload: Cell<u16>,
    pub(super) oamadd_addr: Cell<u16>,
    pub(super) oam: [u8; OAM_SIZE],
    pub(super) oam_writebuf: u8,
    pub(super) oam_priority: bool,

    pub(super) inidisp: u8,
    pub(super) setini: u8,

    // Window settings
    pub(super) w1_left: u8,
    pub(super) w1_right: u8,
    pub(super) w2_left: u8,
    pub(super) w2_right: u8,
    pub(super) tmw: u8,
    pub(super) tsw: u8,
    pub(super) w12sel: u8,
    pub(super) w34sel: u8,
    pub(super) wobjsel: u8,
    pub(super) wbglog: u8,
    pub(super) wobjlog: u8,

    // Color math settings
    pub(super) cgwsel: u8,
    pub(super) cgadsub: u8,
    pub(super) coldata: SnesColor,

    // Mode 7 registers
    pub(super) m7sel: u8,
    pub(super) m7a: i16,
    pub(super) m7b_8b: i8,
    pub(super) m7b: i16,
    pub(super) m7c: i16,
    pub(super) m7d: i16,
    pub(super) m7x: i16,
    pub(super) m7y: i16,
    pub(super) m7hofs: i16,
    pub(super) m7vofs: i16,
    pub(super) m7_old: u8,
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

impl PPUState {
    pub fn new() -> Self {
        Self {
            dbg_layermask: 0,

            vram: vec![0; VRAM_WORDS],
            vmadd: Cell::new(0),
            vmain: 0,
            vram_prefetch: Cell::new(0),

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
            oamadd_addr: Cell::new(0),
            oamadd_reload: Cell::new(0),
            oam: [0; OAM_SIZE],
            oam_writebuf: 0,
            oam_priority: false,

            inidisp: 0,
            setini: 0,

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

            m7sel: 0,
            m7a: 0,
            m7b: 0,
            m7b_8b: 0,
            m7c: 0,
            m7d: 0,
            m7hofs: 0,
            m7vofs: 0,
            m7x: 0,
            m7y: 0,
            m7_old: 0,
        }
    }

    pub(super) fn vram_autoinc(&self, upper: bool) {
        let inc_on_upper = self.vmain & VMAIN_HIGH != 0;
        if upper != inc_on_upper {
            return;
        }

        // Prefetch glitch: prefetch is updated BEFORE the address
        self.vram_update_prefetch();

        let inc = match self.vmain & VMAIN_INC_MASK {
            0 => 1,
            1 => 32,
            2..=3 => 128,
            _ => unreachable!(),
        };
        self.vmadd.set(self.vmadd.get().wrapping_add(inc));
    }

    pub(super) fn vram_update_prefetch(&self) {
        let addr = self.vram_addr_translate(self.vmadd.get());
        self.vram_prefetch
            .set(self.vram[addr as usize & VRAM_ADDRMASK]);
    }

    pub(super) fn vram_addr_translate(&self, addr: u16) -> u16 {
        // Translation  Bitmap Type              Port [2116h/17h]     VRAM Word-Address
        //  8bit rotate  4-color; 1 word/plane   aaaaaaaaYYYxxxxx --> aaaaaaaaxxxxxYYY
        //  9bit rotate  16-color; 2 words/plane aaaaaaaYYYxxxxxP --> aaaaaaaxxxxxPYYY
        // 10bit rotate 256-color; 4 words/plane aaaaaaYYYxxxxxPP --> aaaaaaxxxxxPPYYY
        match (self.vmain >> VMAIN_TRANSLATE_SHIFT) & VMAIN_TRANSLATE_MASK {
            0 => addr,
            1 => (addr & 0xFF00) | ((addr << 3) & 0x00F8) | ((addr >> 5) & 0x07),
            2 => (addr & 0xFE00) | ((addr << 3) & 0x01F8) | ((addr >> 6) & 0x07),
            3 => (addr & 0xFC00) | ((addr << 3) & 0x03F8) | ((addr >> 7) & 0x07),
            _ => unreachable!(),
        }
    }

    pub(super) fn get_bg_map_addr(&self, bg: usize) -> usize {
        debug_assert!((0..4).contains(&bg));
        (self.bgxsc[bg] >> 2) as usize * 1024
    }

    pub(super) fn get_tilemap_offset(&self, bg: usize, tileidx: usize) -> usize {
        // Each map entry is one word.
        let map_addr = self.get_bg_map_addr(bg);
        (map_addr + tileidx) & VRAM_ADDRMASK
    }

    pub(super) fn get_tilemap_entry(&self, bg: usize, tileidx: usize) -> TilemapEntry {
        TilemapEntry(self.vram[self.get_tilemap_offset(bg, tileidx)])
    }

    pub(super) fn get_screen_mode(&self) -> u8 {
        self.bgmode & 0x07
    }

    pub(super) fn get_tilemap_dimensions(&self, bg: usize) -> TilemapDimensions {
        TilemapDimensions::from_u8(self.bgxsc[bg] & 0x03).unwrap()
    }

    pub(super) fn get_tilemap_entry_xy(&self, bg: usize, x: usize, y: usize) -> TilemapEntry {
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

    pub(super) fn get_layer_bpp(&self, bg: usize) -> BPP {
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
    pub(super) fn get_bg_tile<'a>(
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

    pub(super) fn get_bg_tile_size(&self, bg: usize) -> usize {
        debug_assert!((0..4).contains(&bg));
        if self.bgmode & (1 << (4 + bg)) != 0 {
            16
        } else {
            8
        }
    }
}
