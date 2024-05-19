use std::cell::Cell;
use std::ops::Range;
use std::sync::Arc;

use anyhow::Result;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use serbia::serbia;
use serde::{Deserialize, Serialize};

use super::color::SnesColor;
use super::ppu::*;
use super::tile::{Tile, TILE_HEIGHT, TILE_WIDTH};

pub type CgramWord = u16;
pub const CGRAM_WORDS: usize = 256;
#[allow(dead_code)]
pub const CGRAM_WORDSIZE: usize = 2;
#[allow(dead_code)]
pub const CGRAM_ADDRMASK: usize = CGRAM_WORDS - 1;

const OAM_SIZE: usize = 512 + 32;

/// BG3 contains offset-per-tile data for mode 2, 4, 6
const OPT_BG: usize = 2;

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
    pub fn raw(&self) -> u16 {
        self.0
    }

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

    pub fn as_opt(self) -> OptEntry {
        OptEntry(self.0)
    }
}

pub struct OptEntry(u16);
impl OptEntry {
    pub fn scrollv(&self) -> usize {
        (self.0 & 0x3FF) as usize
    }

    pub fn scrollh(&self) -> usize {
        (self.0 & 0x3F8) as usize
    }

    pub fn is_enabled(&self, bg: usize) -> bool {
        debug_assert!(bg < 2);
        (self.0 & (1 << (13 + bg))) != 0
    }

    pub fn is_vertical(&self) -> bool {
        // Only mode 4!
        (self.0 & (1 << 15)) != 0
    }
}

#[derive(Clone, Copy, Debug, ToPrimitive, Eq, PartialEq)]
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
#[derive(Clone, Serialize, Deserialize)]
pub struct PPUState {
    // Debug toggles to mask certain bg/obj layers
    pub dbg_layermask: u8,

    pub(super) vram: Vram,

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
    data_range: Range<usize>,
    map: &'a TilemapEntry,
    bpp: BPP,
}
impl<'a, 'tdata> Tile<'tdata> for BgTile<'a>
where
    'a: 'tdata,
{
    fn get_vram_range(&self) -> Range<usize> {
        self.data_range.clone()
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

            vram: Arc::new(vec![0; VRAM_WORDS]),

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

    pub(super) fn get_bg_map_addr(&self, bg: usize) -> usize {
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

    /// Returns the active PPU mode.
    pub(super) fn get_screen_mode(&self) -> u8 {
        self.bgmode & 0x07
    }

    /// Returns if the PPU is currently in horizontal high res mode.
    pub(super) fn in_highres_h(&self) -> bool {
        match self.get_screen_mode() {
            5 | 6 => true,
            _ => false,
        }
    }

    /// Returns if the PPU is currently in vertical high res mode.
    pub(super) fn in_highres_v(&self) -> bool {
        self.in_highres_h() && self.setini & 1 != 0
    }

    /// Returns the horizontal scaling factor of the active PPU mode,
    /// for backgrounds, based on the full resolution (512).
    /// Does not look at pseudo high res.
    pub(super) fn get_screen_mode_scale_bg(&self) -> usize {
        if self.in_highres_h() {
            1
        } else {
            2
        }
    }

    /// Returns the horizontal scaling factor of the active PPU mode,
    /// for sprites, based on the full resolution (512).
    /// Does not look at pseudo high res.
    pub(super) fn get_screen_mode_scale_sprites(&self) -> usize {
        if self.in_highres_h() {
            2
        } else {
            1
        }
    }

    pub(super) fn get_tilemap_dimensions(&self, bg: usize) -> TilemapDimensions {
        TilemapDimensions::from_u8(self.bgxsc[bg] & 0x03).unwrap()
    }

    pub(super) fn get_tilemap_entry_xy(
        &self,
        bg: usize,
        x: usize,
        y: usize,
        bghofs: usize,
        bgvofs: usize,
    ) -> TilemapEntry {
        debug_assert!(self.get_screen_mode() != 7);

        let (tilewidth, tileheight) = self.get_bg_tile_size(bg);

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
        let tm_x = (bghofs + x) / tilewidth;
        let tm_y = (bgvofs + y) / tileheight;

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

    /// Retrieves offset data for offset-per-tile modes for a horizontal
    /// coordinate.
    pub(super) fn get_opt_entries(&self, bg: usize, x: usize) -> (OptEntry, OptEntry) {
        // OPT affects subtiles, so this is always the subtile size.
        let tilesize = 8;
        let bghofs = self.bgxhofs[bg] as usize;
        let (opthofs, optvofs) = (self.bgxhofs[OPT_BG] as usize, self.bgxvofs[OPT_BG] as usize);

        // First column has no OPT. Index 0 = second column of tiles
        if (bghofs + x) / tilesize == 0 {
            return (OptEntry(0), OptEntry(0));
        }

        (
            self.get_tilemap_entry_xy(OPT_BG, x.saturating_sub(tilesize), 0, opthofs, optvofs)
                .as_opt(),
            self.get_tilemap_entry_xy(
                OPT_BG,
                x.saturating_sub(tilesize),
                tilesize,
                opthofs,
                optvofs,
            )
            .as_opt(),
        )
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
            4 => match bg {
                0 => BPP::Eight,
                1 => BPP::Two,
                _ => unreachable!(),
            },
            5 => match bg {
                0 => BPP::Four,
                1 => BPP::Two,
                _ => unreachable!(),
            },
            6 => match bg {
                0 => BPP::Four,
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
        let (tilewidth, tileheight) = self.get_bg_tile_size(bg);
        if tilewidth == 16 && (px_x >= TILE_WIDTH) == !entry.flip_x() {
            tilenr += 1;
        }
        if tileheight == 16 && (px_y >= TILE_HEIGHT) == !entry.flip_y() {
            tilenr += 16;
        }

        let idx = ((self.bgxnba[bg] as usize * 4096) + (tilenr * len)) & VRAM_ADDRMASK;
        BgTile {
            data_range: idx..(idx + len),
            bpp,
            map: entry,
        }
    }

    /// Returns the size in pixels (width, height) of a bg tile
    pub(super) fn get_bg_tile_size(&self, bg: usize) -> (usize, usize) {
        match self.get_screen_mode() {
            0..=5 if self.bgmode & (1 << (4 + bg)) != 0 => (16, 16),
            0..=4 => (8, 8),
            5 => (16, 8),
            6 => (16, 16),
            7 => (8, 8),
            _ => unreachable!(),
        }
    }
}
