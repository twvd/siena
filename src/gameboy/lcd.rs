use crate::gameboy::bus::bus::BusMember;
use crate::gameboy::lcd_oam::{OAMTable, ObjPriMode};
use crate::gameboy::tickable::{Tickable, Ticks};

use anyhow::Result;
use crossbeam_channel::{Receiver, Sender, TrySendError};
use num_derive::ToPrimitive;
use num_traits::ToPrimitive;
use strum::EnumCount;
use strum_macros::EnumCount as EnumCountMacro;

use std::cmp;

pub const LCD_W: usize = 160;
pub const LCD_H: usize = 144;

/// Type of a (palleted) color definition (RGB555)
pub type Color = u16;

/// Type of a color index
/// Valid values: 0 - 3
pub type ColorIndex = u8;
const COLORINDEX_DEFAULT: ColorIndex = 0;

const VRAM_SIZE: usize = 0x2000;
const VRAM_BANKS: usize = 2;

// Tile sizes
const TILE_BSIZE: usize = 16;
const TILE_W: isize = 8;
const TILE_H: isize = 8;

// Background/window size (in tiles)
const BGW_H: isize = 32;
const BGW_W: isize = 32;

// LCDC flags
const LCDC_ENABLE: u8 = 1 << 7;
const LCDC_WINDOW_TILEMAP: u8 = 1 << 6;
const LCDC_WINDOW_ENABLE: u8 = 1 << 5;
const LCDC_BGW_TILEDATA: u8 = 1 << 4;
const LCDC_BG_TILEMAP: u8 = 1 << 3;
const LCDC_OBJ_SIZE: u8 = 1 << 2;
const LCDC_OBJ_ENABLE: u8 = 1 << 1;
const LCDC_BGW_ENABLE: u8 = 1 << 0;
const LCDC_CGB_BGW_MASTER_PRIORITY: u8 = 1 << 0;

// Writable LCDS bits
const LCDS_MASK: u8 = 0x78;

const LCDS_STATMODE_MASK: u8 = 0x03;

// LCDS bits
const LCDS_INT_LYC: u8 = 1 << 6;
const LCDS_INT_STAT_OAM: u8 = 1 << 5;
const LCDS_INT_STAT_VBLANK: u8 = 1 << 4;
const LCDS_INT_STAT_HBLANK: u8 = 1 << 3;
const LCDS_LYC: u8 = 1 << 2;

// OAM flags
const OAM_FLIP_Y: u8 = 1 << 6;
const OAM_VRAM_BANK: u8 = 1 << 3;

// Gameboy Color register properties
const CRAM_ENTRIES: usize = 0x20;
const XCPS_ADDR_MASK: u8 = 0x3F;
const XCPS_AUTO_INC: u8 = 1 << 7;
const COLOR_MASK: Color = 0x7FFF;
const COLOR_DEFAULT: Color = 0x7FFF; // White
const CGB_PALETTE_SIZE: usize = 4;

// OAM/BG map attributes
const TILEATTR_PALETTE_CGB_MASK: u8 = 0x07;
const TILEATTR_PALETTE_CGB_SHIFT: u8 = 0;
const TILEATTR_PALETTE_DMG_MASK: u8 = 1 << 4;
const TILEATTR_PALETTE_DMG_SHIFT: u8 = 4;
const TILEATTR_VRAM_BANK: u8 = 1 << 3;

/// Generic of the DMG and CGB palette types
#[derive(Copy, Clone)]
enum Palette {
    DMG(u8),
    CGB([Color; CGB_PALETTE_SIZE]),
}

impl Palette {
    /// Converts a color index to a color from this palette
    fn get_color(&self, cidx: u8) -> Color {
        match self {
            Palette::DMG(p) => ((p >> (cidx * 2)) & 3) as Color,
            //match (p >> (cidx * 2)) & 3 {
            //    3 => 0,
            //    2 => 0b01000_01000_01000,
            //    1 => 0b11000_11000_11000,
            //    0 => COLOR_DEFAULT,
            //    _ => unreachable!(),
            //},
            Palette::CGB(p) => p[cidx as usize],
        }
    }
}

/// Current state of a dot while rendering
#[derive(Copy, Clone)]
struct DotState {
    /// Paletted color
    color: Color,

    /// Color index
    idx: ColorIndex,

    /// Priority bit was set
    priority: bool,
}

impl DotState {
    fn new() -> Self {
        Self {
            color: COLOR_DEFAULT,
            idx: COLORINDEX_DEFAULT,
            priority: false,
        }
    }
}

/// Tile types
#[derive(Eq, PartialEq)]
enum TileType {
    Background,
    Window,
    Object,
}

/// Generic abstraction of bg/window/object tile + attributes
struct Tile<'a> {
    data: &'a [u8],
    attr: u8,
    ttype: TileType,
}

impl<'a> Tile<'a> {
    pub fn new(data: &'a [u8], attr: u8, ttype: TileType) -> Self {
        Self { data, attr, ttype }
    }

    fn flip_x(&self) -> bool {
        (self.attr & (1 << 5)) != 0
    }

    fn flip_y(&self) -> bool {
        (self.attr & (1 << 6)) != 0
    }

    fn has_priority(&self) -> bool {
        (self.attr & (1 << 7)) != 0
    }

    fn is_object(&self) -> bool {
        self.ttype == TileType::Object
    }

    fn is_bg(&self) -> bool {
        self.ttype == TileType::Background
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, ToPrimitive)]
pub enum LCDStatMode {
    Search = 2,
    Transfer = 3,
    HBlank = 0,
    VBlank = 1,
}

#[derive(Debug, EnumCountMacro, ToPrimitive)]
enum RegHist {
    BGP,
    SCX,
    SCY,
}

/// LCD controller state
pub struct LCDController {
    /// OAM memory
    oam: OAMTable,

    /// Gameboy Color mode
    cgb: bool,

    /// VRAM memory
    vram: [u8; VRAM_SIZE * VRAM_BANKS],

    /// LCDC - LCD Control register
    lcdc: u8,

    /// LCDS - LCD Status register
    lcds: u8,

    /// SCY - Scroll Y register
    scy: u8,

    /// SCX - Scroll X register
    scx: u8,

    /// WX - Window X register
    wx: u8,

    /// WY - Window Y register
    wy: u8,

    /// Current scanline
    ly: u8,

    /// Current window scanline
    wly: u8,

    /// LY compare register
    lyc: u8,

    /// Background/window palette (DMG)
    bgp: u8,

    /// Object Palettes (DMG)
    obp: [u8; 2],

    /// Background CRAM (CGB)
    cram_bg: [Color; CRAM_ENTRIES],

    /// Object CRAM (CGB)
    cram_obj: [Color; CRAM_ENTRIES],

    /// Background Color Palette Specification (BCPS) (CGB)
    /// Bit 7: auto increment
    /// Bit 5-0: address (XCPS_ADDR_MASK)
    bcps: u8,

    /// Object Color Palette Specification (OCPS)(CGB)
    /// Bit 7: auto increment
    /// Bit 5-0: address (XCPS_ADDR_MASK)
    ocps: u8,

    /// VRAM bank select
    vbk: u8,

    /// Output display needs updsting
    redraw_pending: bool,

    /// Dot refresh position
    dots: u128,

    /// STAT interrupt request
    intreq_stat: bool,

    /// VBlank interrupt request
    intreq_vblank: bool,

    /// STAT interrupt line
    stat_int_line: bool,

    /// Object priority mode
    objpri: ObjPriMode,

    /// Skip drawing X frames
    skip_frames: usize,

    /// Register change history during mode 3
    reg_history: [[u8; Self::TRANSFER_PERIOD as usize]; RegHist::COUNT],

    /// Scanline output channel
    line_output: Sender<(usize, Vec<Color>)>,

    /// Receiver for scanline output channel (requestable once)
    line_output_recv: Option<Receiver<(usize, Vec<Color>)>>,
}

impl LCDController {
    /// Dots per scanline (including HBlank)
    const DOTS_PER_LINE: u128 = 456;

    /// Amount of vertical scanlines (including VBlank)
    const SCANLINES: u128 = 154;

    /// Start scanline of VBLANK period
    const VBLANK_START: u128 = 144;

    /// Amount of dots in 'search' mode
    const SEARCH_PERIOD: u128 = 80;

    /// Amount of dots in 'transfer' mode (actually 168 to 291)
    const TRANSFER_PERIOD: u128 = 200;

    /// Initialization of 'dots' after LCD is enabled
    const DOTS_INIT: u128 = 4;

    pub fn new(cgb: bool) -> Self {
        let objpri = if cgb {
            ObjPriMode::OAMPosition
        } else {
            ObjPriMode::Coordinate
        };

        let (sender, receiver) = crossbeam_channel::unbounded();

        let mut r = Self {
            cgb,
            oam: OAMTable::new(),
            vram: [0; VRAM_SIZE * VRAM_BANKS],

            lcdc: LCDC_ENABLE,
            lcds: LCDStatMode::Search.to_u8().unwrap(),
            scy: 0,
            scx: 0,
            wx: 0,
            wy: 0,
            wly: 0,
            ly: 0,
            lyc: 0,
            bgp: 0,
            obp: [0, 0],
            bcps: 0,
            ocps: 0,
            vbk: 0,
            cram_bg: [0x7F; CRAM_ENTRIES],
            cram_obj: [0; CRAM_ENTRIES],

            redraw_pending: false,

            dots: Self::DOTS_INIT,

            intreq_stat: false,
            intreq_vblank: false,
            stat_int_line: false,

            objpri,
            skip_frames: 1,

            reg_history: [[0; Self::TRANSFER_PERIOD as usize]; RegHist::COUNT],

            line_output: sender,
            line_output_recv: Some(receiver),
        };
        r.reset();

        r
    }

    /// Gets the scanline output channel receiver
    /// (can be used once)
    pub fn get_scanline_output(&mut self) -> Receiver<(usize, Vec<Color>)> {
        assert!(self.line_output_recv.is_some());
        std::mem::replace(&mut self.line_output_recv, None).unwrap()
    }

    /// Gets current stat mode based on the dot clock
    pub fn get_stat_mode(&self) -> LCDStatMode {
        // Mode 2  2_____2_____2_____2_____2_____2___________________2____
        // Mode 3  _33____33____33____33____33____33__________________3___
        // Mode 0  ___000___000___000___000___000___000________________000
        // Mode 1  ____________________________________11111111111111_____
        if self.in_vblank() {
            LCDStatMode::VBlank
        } else {
            let hpos = self.dots % Self::DOTS_PER_LINE;
            if hpos < Self::SEARCH_PERIOD {
                LCDStatMode::Search
            } else if hpos < Self::SEARCH_PERIOD + Self::TRANSFER_PERIOD {
                LCDStatMode::Transfer
            } else {
                LCDStatMode::HBlank
            }
        }
    }

    /// Record a register change for something tracked during mode 3
    fn record_reg(&mut self, reg: RegHist, val: u8) {
        if self.get_stat_mode() == LCDStatMode::Transfer {
            let transfer_cycle = cmp::min(
                ((self.dots % Self::DOTS_PER_LINE) - Self::SEARCH_PERIOD) as usize,
                Self::TRANSFER_PERIOD as usize,
            );

            // Fill from this cycle onwards
            self.reg_history[reg.to_usize().unwrap()][transfer_cycle..].fill(val);
        } else {
            // Fill for the entire line
            self.reg_history[reg.to_usize().unwrap()].fill(val);
        }
    }

    /// Fetch a register for something tracked during mode 3
    fn fetch_reg(&self, reg: RegHist, x: usize) -> u8 {
        assert_eq!(self.get_stat_mode(), LCDStatMode::Transfer);
        let transfer_cycle = cmp::min(
            ((self.dots % Self::DOTS_PER_LINE) - Self::SEARCH_PERIOD) as usize + x,
            Self::TRANSFER_PERIOD as usize - 1,
        );
        self.reg_history[reg.to_usize().unwrap()][transfer_cycle]
    }

    /// Calculate LY based on current timed LCD scan
    fn calc_ly(&self) -> u8 {
        Self::calc_scanline(self.dots)
    }

    fn calc_scanline(dots: u128) -> u8 {
        let lines_scanned = dots / Self::DOTS_PER_LINE;
        (lines_scanned % Self::SCANLINES) as u8
    }

    pub fn in_vblank(&self) -> bool {
        self.dots >= (Self::VBLANK_START * Self::DOTS_PER_LINE)
    }

    /// Tests all conditions for the window to be drawn and the counter
    /// running
    fn is_window_active(&self) -> bool {
        self.lcdc & LCDC_WINDOW_ENABLE == LCDC_WINDOW_ENABLE
            && (self.cgb || self.lcdc & LCDC_BGW_ENABLE == LCDC_BGW_ENABLE)
            && (0u8..=166).contains(&self.wx)
            && (0u8..=143).contains(&self.wy)
    }

    fn get_bgw_tile(&self, tm_x: isize, tm_y: isize, ttype: TileType) -> Tile {
        let selbit = match ttype {
            TileType::Background => LCDC_BG_TILEMAP,
            TileType::Window => LCDC_WINDOW_TILEMAP,
            _ => unreachable!(),
        };
        assert!(tm_x < BGW_W && tm_y < BGW_H);

        // VRAM offset = 8000 - 9FFF
        // BG tile map at 9800 - 9BFF or 9C00 - 9FFF
        // Window tile map at 9800 - 9BFF or 9C00 - 9FFF
        let map_offset: isize = if self.lcdc & selbit != 0 {
            0x9C00
        } else {
            0x9800
        };

        let tile_id = self.vram[(map_offset - 0x8000 + (tm_y * BGW_H) + tm_x) as usize] as usize;

        // Tile attributes on BG/Window tiles is CGB-only.
        let tile_attr = if !self.cgb {
            0
        } else {
            self.vram[VRAM_SIZE + (map_offset - 0x8000 + (tm_y * BGW_H) + tm_x) as usize]
        };

        // CGB has two VRAM banks, selectable through attributes
        let tile_bank_offset = if self.cgb && (tile_attr & TILEATTR_VRAM_BANK) != 0 {
            VRAM_SIZE
        } else {
            0
        };

        // VRAM offset = 8000 - 9FFF
        // BG/Win tile data at 8800 - 97FF and 8000 - 8FFF
        // BG/Win tiles always 8 x 8 pixels
        let tile_addr = if self.lcdc & LCDC_BGW_TILEDATA != 0 {
            // 0x8000 base offset, contiguous blocks
            0x8000 + tile_id * TILE_BSIZE
        } else {
            // 0-127 from 0x9000, 128-255 from 0x8800
            if tile_id < 128 {
                0x9000 + tile_id * TILE_BSIZE
            } else {
                0x8800 + (tile_id - 128) * TILE_BSIZE
            }
        };

        // Correct for our VRAM array
        let tile_addr = (tile_addr - 0x8000) as usize + tile_bank_offset;

        Tile::new(
            &self.vram[tile_addr..tile_addr + TILE_BSIZE],
            tile_attr,
            ttype,
        )
    }

    fn get_obj_tile(&self, tile_idx: usize, oam_flags: u8) -> Tile {
        // VRAM offset = 8000 - 9FFF
        // Sprites always start from 8000 (tile_idx 0)
        // Sprites can be 8x8 or 8x16 (LCDC_OBJ_SIZE)
        // In 8x16 mode, the least significant bit of tile_idx
        // is ignored.
        let offset = 0x8000;
        let tile_bank_offset = if self.cgb && (oam_flags & OAM_VRAM_BANK) == OAM_VRAM_BANK {
            VRAM_SIZE
        } else {
            0
        };
        let tile_addr = offset - 0x8000 + tile_idx * TILE_BSIZE + tile_bank_offset;

        Tile::new(
            &self.vram[tile_addr..tile_addr + TILE_BSIZE],
            oam_flags,
            TileType::Object,
        )
    }

    fn tile_decode(tile: &[u8], x: usize, y: usize) -> u8 {
        // Least significant bit in the odd bytes,
        // most significant bit in the even bytes.
        let x = 7 - x;
        let lsb = (tile[y * 2] & (1 << x)) >> x;
        let msb = (tile[y * 2 + 1] & (1 << x)) >> x;

        lsb | msb << 1
    }

    fn write_xcpd(cram: &mut [Color], xcps: &mut u8, val: u8) {
        let addr: usize = (*xcps & XCPS_ADDR_MASK) as usize;
        let entry: usize = addr >> 1;
        let new_val: Color = if addr & 1 == 0 {
            // Write LSB
            cram[entry] & 0xFF00 | val as Color
        } else {
            // Write MSB
            cram[entry] & 0x00FF | ((val as Color) << 8)
        } & COLOR_MASK;
        cram[entry] = new_val;

        // Handle auto-increment
        if *xcps & XCPS_AUTO_INC == XCPS_AUTO_INC {
            let new_addr = (*xcps + 1) & XCPS_ADDR_MASK;
            *xcps = new_addr | XCPS_AUTO_INC;
        }
    }

    fn read_xcpd(cram: &[Color], xcps: &u8) -> u8 {
        let addr: usize = (*xcps & XCPS_ADDR_MASK) as usize;
        let entry: usize = addr >> 1;
        if addr & 1 == 0 {
            // Read LSB
            (cram[entry] & 0xFF) as u8
        } else {
            // Read MSB
            (cram[entry] >> 8) as u8
        }

        // Auto-increment has no effect on reads
    }

    fn get_tile_palette(&self, tile: &Tile, x: usize) -> Palette {
        if !self.cgb {
            let palette_val = match tile.ttype {
                TileType::Background | TileType::Window => self.fetch_reg(RegHist::BGP, x),
                TileType::Object => {
                    self.obp[((tile.attr & TILEATTR_PALETTE_DMG_MASK) >> TILEATTR_PALETTE_DMG_SHIFT)
                        as usize]
                }
            };

            return Palette::DMG(palette_val);
        }

        // For CGB, consult CRAM.
        let palidx =
            ((tile.attr & TILEATTR_PALETTE_CGB_MASK) >> TILEATTR_PALETTE_CGB_SHIFT) as usize;
        let cram_offset = (palidx * CGB_PALETTE_SIZE)..((palidx + 1) * CGB_PALETTE_SIZE);
        let cram_val: [Color; CGB_PALETTE_SIZE] = match tile.ttype {
            TileType::Background | TileType::Window => {
                self.cram_bg[cram_offset].try_into().unwrap()
            }
            TileType::Object => self.cram_obj[cram_offset].try_into().unwrap(),
        };

        Palette::CGB(cram_val)
    }

    fn draw_tile_at(
        &self,
        tile: &Tile,
        line: &mut [DotState],
        x: isize,
        y: isize,
        scanline: isize,
    ) {
        for ty in 0..TILE_H {
            // Y-axis wrap around (scrolling)
            let disp_y = if tile.is_bg() {
                (y + ty).rem_euclid(BGW_H * TILE_H)
            } else {
                y + ty
            };

            if disp_y != scanline {
                continue;
            }

            for tx in 0..TILE_W {
                // X-axis wrap around (scrolling)
                let disp_x = if tile.is_bg() {
                    (x + tx).rem_euclid(BGW_W * TILE_W)
                } else {
                    x + tx
                };

                if disp_x < 0 || disp_x >= LCD_W as isize {
                    continue;
                }

                let palette = self.get_tile_palette(&tile, disp_x as usize);
                let color_idx = Self::tile_decode(
                    &tile.data,
                    if (self.cgb || tile.is_object()) && tile.flip_x() {
                        // Mirror along X axis
                        7 - tx as usize
                    } else {
                        tx as usize
                    },
                    if (self.cgb || tile.is_object()) && tile.flip_y() {
                        // Mirror along Y axis
                        7 - ty as usize
                    } else {
                        ty as usize
                    },
                );

                // Objects blend into background
                if tile.is_object() {
                    if color_idx == 0 {
                        continue;
                    }

                    // BG priority
                    if !self.cgb {
                        if tile.has_priority() && line[disp_x as usize].idx != COLORINDEX_DEFAULT {
                            continue;
                        }
                    } else {
                        if line[disp_x as usize].idx != COLORINDEX_DEFAULT
                            && (self.lcdc & LCDC_CGB_BGW_MASTER_PRIORITY
                                == LCDC_CGB_BGW_MASTER_PRIORITY)
                            && (tile.has_priority() || line[disp_x as usize].priority)
                        {
                            continue;
                        }
                    }
                } else {
                    // Track priority for object blending later
                    line[disp_x as usize].priority = tile.has_priority();
                }

                line[disp_x as usize].color = palette.get_color(color_idx);
                line[disp_x as usize].idx = color_idx;
            }
        }
    }

    pub fn draw_scanline(&mut self, scanline: isize) {
        if self.lcdc & LCDC_ENABLE != LCDC_ENABLE {
            return;
        }

        let mut line = [DotState::new(); LCD_W];

        // Background
        if self.cgb || self.lcdc & LCDC_BGW_ENABLE == LCDC_BGW_ENABLE {
            let t_y = (scanline + self.scy as isize).rem_euclid(BGW_H * TILE_H) / TILE_H;
            for t_x in 0..BGW_W {
                let tile = self.get_bgw_tile(t_x, t_y, TileType::Background);

                let draw_x = (t_x as isize * TILE_W)
                    - self.fetch_reg(RegHist::SCX, t_x as usize * TILE_W as usize) as isize;
                let draw_y = (t_y as isize * TILE_H)
                    - self.fetch_reg(RegHist::SCY, t_x as usize * TILE_W as usize) as isize;

                self.draw_tile_at(&tile, &mut line, draw_x, draw_y, scanline);
            }
        }

        // The window
        if self.is_window_active() && scanline >= self.wy as isize {
            let t_y = self.wly as isize / TILE_H;
            for t_x in 0..BGW_W {
                let tile = self.get_bgw_tile(t_x, t_y, TileType::Window);

                self.draw_tile_at(
                    &tile,
                    &mut line,
                    (t_x as isize * TILE_W) + self.wx as isize - 7,
                    (t_y as isize * TILE_H) + (scanline - self.wly as isize),
                    scanline,
                );
            }
        }

        // Object sprites
        if self.lcdc & LCDC_OBJ_ENABLE == LCDC_OBJ_ENABLE {
            for e in self.oam.iter_scanline(
                scanline,
                if self.lcdc & LCDC_OBJ_SIZE == LCDC_OBJ_SIZE {
                    TILE_H * 2
                } else {
                    TILE_H
                },
                self.objpri,
            ) {
                let disp_x = e.x as isize - 8;
                let disp_y = e.y as isize - 16;
                let mut tile_idx = e.tile_idx as usize;

                if self.lcdc & LCDC_OBJ_SIZE == LCDC_OBJ_SIZE {
                    // Ignore bit 0 for 8x16 objects
                    tile_idx &= !0x01;
                    if e.flags & OAM_FLIP_Y != 0 {
                        // Also rotate the tiles for 8x16
                        tile_idx |= 0x01;
                    }
                }

                let tile = self.get_obj_tile(tile_idx, e.flags);

                self.draw_tile_at(&tile, &mut line, disp_x, disp_y, scanline);

                if self.lcdc & LCDC_OBJ_SIZE == LCDC_OBJ_SIZE {
                    // Draw second tile for 8x16 objects
                    let tile_idx2 = if tile.flip_y() {
                        tile_idx & !0x01
                    } else {
                        tile_idx | 0x01
                    };
                    let tile2 = self.get_obj_tile(tile_idx2, e.flags);
                    self.draw_tile_at(&tile2, &mut line, disp_x, disp_y + TILE_H, scanline);
                }
            }
        }

        // TODO make a nicer interface
        self.line_output
            .send((
                scanline as usize,
                line.into_iter().map(|c| c.color.into()).collect::<Vec<_>>(),
            ))
            .unwrap();

        // Reset current state of tracked registers for next scanline
        self.reg_history[RegHist::BGP.to_usize().unwrap()].fill(self.bgp);
        self.reg_history[RegHist::SCX.to_usize().unwrap()].fill(self.scx);
        self.reg_history[RegHist::SCY.to_usize().unwrap()].fill(self.scy);
    }

    pub fn get_clr_intreq_stat(&mut self) -> bool {
        let b = self.intreq_stat;
        self.intreq_stat = false;
        b
    }

    pub fn get_clr_intreq_vblank(&mut self) -> bool {
        let b = self.intreq_vblank;
        self.intreq_vblank = false;
        b
    }

    /// Tests STAT interrupt conditions, returns if an interrupt should fire.
    fn check_stat_int(&mut self, lcds: u8) -> bool {
        let mode = self.get_stat_mode();
        let new_line = (lcds & LCDS_INT_STAT_VBLANK != 0 && mode == LCDStatMode::VBlank)
            || (lcds & LCDS_INT_STAT_OAM != 0 && mode == LCDStatMode::Search)
            || (lcds & LCDS_INT_STAT_HBLANK != 0 && mode == LCDStatMode::HBlank)
            || (lcds & LCDS_INT_LYC != 0 && self.ly == self.lyc);

        // STAT interrupt blocking
        // If the current interrupt line is high, a new interrupt condition
        // should not trigger another STAT interrupt.
        if !self.stat_int_line && new_line {
            self.stat_int_line = true;
            return true;
        }

        self.stat_int_line = new_line;
        return false;
    }

    /// Re-initializes the PPU (e.g. after being disabled).
    fn reset(&mut self) {
        self.dots = Self::DOTS_INIT;
        self.ly = 0;
        self.lcds = self.lcds & !LCDS_STATMODE_MASK | LCDStatMode::Search.to_u8().unwrap();

        // After the PPU is re-enabled, the first frame is discarded.
        self.skip_frames = 1;
    }
}

impl Tickable for LCDController {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // LCD controller is not affected by double speed
        let ticks = ticks.get_t_no_ds();

        if self.lcdc & LCDC_ENABLE == 0 {
            // PPU disabled
            return Ok(());
        }

        let old_mode = self.get_stat_mode();

        // TODO this may skip interrupts on many ticks?
        assert!(ticks < Self::SEARCH_PERIOD as usize);

        self.dots = (self.dots + ticks as u128) % (Self::DOTS_PER_LINE * Self::SCANLINES);

        let newly = self.calc_ly();
        let new_mode = self.get_stat_mode();

        if newly != self.ly {
            self.ly = newly;

            // Check VBlank interrupt
            if old_mode != LCDStatMode::VBlank && new_mode == LCDStatMode::VBlank {
                self.intreq_vblank = true;

                // Reset window line counter
                self.wly = 0;
            }
        }

        // Draw when in transfer mode
        if old_mode != LCDStatMode::Transfer && new_mode == LCDStatMode::Transfer {
            if !self.in_vblank() {
                self.draw_scanline(self.ly as isize);

                // Window line counter
                if self.is_window_active() && self.ly >= self.wy {
                    self.wly += 1;
                }
            } else {
                self.line_output.send((self.ly as usize, vec![])).unwrap();
            }
        }

        // Update mode register
        self.lcds = (self.lcds & !LCDS_STATMODE_MASK) | self.get_stat_mode().to_u8().unwrap();

        // Check LY compare bit
        if self.ly == self.lyc {
            self.lcds |= LCDS_LYC;
        } else {
            self.lcds &= !LCDS_LYC;
        }

        // Check STAT interrupt
        if self.check_stat_int(self.lcds) {
            self.intreq_stat = true;
        }

        if self.in_vblank() {
            if self.redraw_pending {
                self.redraw_pending = false;
                if self.skip_frames == 0 {
                    // TODO
                    //self.output.render();
                } else {
                    self.skip_frames -= 1;
                }
            }
        } else {
            self.redraw_pending = true;
        }

        Ok(())
    }
}

impl BusMember for LCDController {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // Video RAM
            0x8000..=0x9FFF => self.vram[addr as usize - 0x8000 + (VRAM_SIZE * self.vbk as usize)],

            // Object Attribute Table (OAM)
            0xFE00..=0xFE9F => self.oam.read(addr as usize - 0xFE00),

            // LCDC - LCD control register
            0xFF40 => self.lcdc,

            // LCDS - LCD status register
            0xFF41 => {
                if self.lcdc & LCDC_ENABLE == 0 {
                    self.lcds & !LCDS_STATMODE_MASK
                } else {
                    self.lcds
                }
            }

            // SCY - Background scrolling viewport Y
            0xFF42 => self.scy,

            // SCX - Background scrolling viewport X
            0xFF43 => self.scx,

            // LY - LCD update Y position
            0xFF44 => {
                if self.lcdc & LCDC_ENABLE == 0 {
                    0
                } else {
                    self.ly
                }
            }

            // LYC - LY compare
            0xFF45 => self.lyc,

            // BGP - Background and window palette
            0xFF47 => self.bgp,

            // OBPx - Object palette
            0xFF48 => self.obp[0],
            0xFF49 => self.obp[1],

            // WY - Window Y register
            0xFF4A => self.wy,

            // WX - Window X register
            0xFF4B => self.wx,

            // VBK - VRAM bank select (CGB)
            0xFF4F if self.cgb => self.vbk,

            // BCPS - Background Color Palette Specification
            0xFF68 if self.cgb => self.bcps,

            // BCPD - Background Color Palette Data
            0xFF69 if self.cgb => Self::read_xcpd(&self.cram_bg, &self.bcps),

            // OCPS - Object Color Palette Specification
            0xFF6A if self.cgb => self.ocps,

            // OCPD - Background Color Palette Data
            0xFF6B if self.cgb => Self::read_xcpd(&self.cram_obj, &self.ocps),

            // OPRI - Object Priority Mode
            0xFF6C if self.cgb => {
                0xFE | match self.objpri {
                    ObjPriMode::Coordinate => 1,
                    ObjPriMode::OAMPosition => 0,
                }
            }

            _ => 0xFF,
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            // Video RAM
            0x8000..=0x9FFF => self.vram[addr - 0x8000 + (VRAM_SIZE * self.vbk as usize)] = val,

            // Object Attribute Table (OAM)
            0xFE00..=0xFE9F => self.oam.write(addr - 0xFE00, val),

            // LCDC - LCD control register
            0xFF40 => {
                if self.lcdc & LCDC_ENABLE == 0 && val & LCDC_ENABLE != 0 {
                    // PPU re-enabled
                    self.reset();
                }
                self.lcdc = val;
            }

            // LCDS - LCD status register
            0xFF41 => {
                // DMG hardware has a quirk where the PPU reads LCDS as 0xFF
                // for one cycle when LCDS is written.
                // This can trigger a STAT interrupt if any of the interrupt
                // source conditions is currently true, unless STAT blocking occurs.
                if !self.cgb && self.check_stat_int(0xFF) {
                    self.stat_int_line = true;
                    self.intreq_stat = true;
                }

                self.lcds = (self.lcds & !LCDS_MASK) | (val & LCDS_MASK);
            }

            // SCY - Background scrolling viewport Y
            0xFF42 => {
                self.record_reg(RegHist::SCY, val);
                self.scy = val;
            }

            // SCX - Background scrolling viewport X
            0xFF43 => {
                self.record_reg(RegHist::SCX, val);
                self.scx = val;
            }

            // LYC - LY compare
            0xFF45 => self.lyc = val,

            // BGP - Background and window palette
            0xFF47 => {
                self.record_reg(RegHist::BGP, val);
                self.bgp = val;
            }

            // OBPx - Object Palette
            0xFF48 => self.obp[0] = val,
            0xFF49 => self.obp[1] = val,

            // WY - Window Y register
            0xFF4A => self.wy = val,

            // WX - Window X register
            0xFF4B => self.wx = val,

            // VBK - VRAM bank select (CGB)
            0xFF4F if self.cgb => self.vbk = val & 1,

            // BCPS - Background Color Palette Specification
            0xFF68 if self.cgb => self.bcps = (val & XCPS_ADDR_MASK) | (val & XCPS_AUTO_INC),

            // BCPD - Background Color Palette Data
            0xFF69 if self.cgb => Self::write_xcpd(&mut self.cram_bg, &mut self.bcps, val),

            // OCPS - Object Color Palette Specification
            0xFF6A if self.cgb => self.ocps = (val & XCPS_ADDR_MASK) | (val & XCPS_AUTO_INC),

            // OCPD - Background Color Palette Data
            0xFF6B if self.cgb => Self::write_xcpd(&mut self.cram_obj, &mut self.ocps, val),

            // OPRI - Object Priority Mode
            0xFF6C if self.cgb => {
                self.objpri = if val & 0x01 == 0x01 {
                    ObjPriMode::Coordinate
                } else {
                    ObjPriMode::OAMPosition
                }
            }

            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lcd() -> LCDController {
        LCDController::new(false)
    }

    fn lcd_cgb() -> LCDController {
        LCDController::new(true)
    }

    #[test]
    fn tile_decode() {
        let tile = [0x3C, 0x7E];
        let result = [0, 2, 3, 3, 3, 3, 2, 0];

        for x in 0..result.len() {
            assert_eq!(LCDController::tile_decode(&tile, x, 0), result[x]);
        }
    }

    #[test]
    fn statmode() {
        fn next(l: &mut LCDController) {
            let val = l.get_stat_mode();
            while l.get_stat_mode() == val {
                l.tick(Ticks::from_t(1)).unwrap();
            }
        }

        let mut c = lcd();
        assert_eq!(c.get_stat_mode(), LCDStatMode::Search);

        for _ in 0..LCDController::VBLANK_START {
            assert_eq!(c.get_stat_mode(), LCDStatMode::Search);
            next(&mut c);
            assert_eq!(c.get_stat_mode(), LCDStatMode::Transfer);
            next(&mut c);
            assert_eq!(c.get_stat_mode(), LCDStatMode::HBlank);
            next(&mut c);
        }
        assert_eq!(c.get_stat_mode(), LCDStatMode::VBlank);
    }

    #[test]
    fn vblank() {
        let mut c = lcd();
        for _ in
            (LCDController::DOTS_INIT as usize)..(LCD_H * LCDController::DOTS_PER_LINE as usize)
        {
            assert!(!c.in_vblank());
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.in_vblank());
    }

    #[test]
    fn int_stat_lyc() {
        let mut c = lcd();
        c.write(0xFF45, 10);
        c.write(0xFF41, LCDS_INT_LYC);
        c.get_clr_intreq_stat(); // Clear STAT write glitch

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());
        assert!(c.read(0xFF41) & LCDS_LYC != LCDS_LYC);

        while c.ly != 10 {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.read(0xFF41) & LCDS_LYC == LCDS_LYC);
        assert!(c.get_clr_intreq_stat());
        assert!(!c.get_clr_intreq_stat());

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(c.read(0xFF41) & LCDS_LYC == LCDS_LYC);
        assert!(!c.get_clr_intreq_stat());
    }

    #[test]
    fn int_stat_vblank() {
        let mut c = lcd();
        c.write(0xFF41, LCDS_INT_STAT_VBLANK);
        c.get_clr_intreq_stat(); // Clear STAT write glitch

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());

        while !c.in_vblank() {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.get_clr_intreq_stat());
        assert!(!c.get_clr_intreq_stat());

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());
    }

    #[test]
    fn int_stat_hblank() {
        let mut c = lcd();
        c.write(0xFF41, LCDS_INT_STAT_HBLANK);
        c.get_clr_intreq_stat(); // Clear STAT write glitch

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());

        while c.get_stat_mode() != LCDStatMode::HBlank {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.get_clr_intreq_stat());
        assert!(!c.get_clr_intreq_stat());

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());
    }

    #[test]
    fn int_stat_oam() {
        let mut c = lcd();

        while c.get_stat_mode() == LCDStatMode::Search {
            c.tick(Ticks::from_t(1)).unwrap();
        }

        c.write(0xFF41, LCDS_INT_STAT_OAM);
        c.get_clr_intreq_stat(); // Clear STAT write glitch

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());

        while c.get_stat_mode() != LCDStatMode::Search {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.get_clr_intreq_stat());
        assert!(!c.get_clr_intreq_stat());

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_stat());
    }

    #[test]
    fn int_stat_quirk() {
        let mut c = lcd();

        assert_eq!(c.get_stat_mode(), LCDStatMode::Search);

        c.write(0xFF45, 1); // LYC
        assert!(!c.get_clr_intreq_stat());
        c.write(0xFF41, 0);
        // Triggered by LY = 0, OAM
        assert!(c.get_clr_intreq_stat());

        while c.get_stat_mode() != LCDStatMode::Transfer {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        // NOT triggered by LY = 0, transfer
        c.write(0xFF41, 0);
        assert!(!c.get_clr_intreq_stat());

        while c.get_stat_mode() != LCDStatMode::HBlank {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        // Triggered by LY = 0, HBlank
        c.write(0xFF41, 0);
        assert!(c.get_clr_intreq_stat());

        while c.get_stat_mode() != LCDStatMode::Transfer {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        // Triggered by LY = 1, LY=LYC
        c.write(0xFF41, 0);
        assert!(c.get_clr_intreq_stat());

        while c.get_stat_mode() != LCDStatMode::VBlank {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        // Triggered by VBlank
        c.write(0xFF41, 0);
        assert!(c.get_clr_intreq_stat());
    }

    #[test]
    fn int_vblank() {
        let mut c = lcd();

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_vblank());

        while !c.in_vblank() {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.get_clr_intreq_vblank());
        assert!(!c.get_clr_intreq_vblank());

        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_vblank());
    }

    #[test]
    fn int_vblank_lcdc_disable() {
        let mut c = lcd();

        c.write(0xFF40, LCDC_ENABLE);
        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_vblank());

        for _ in 0..(LCDController::DOTS_PER_LINE * LCDController::SCANLINES) {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(c.get_clr_intreq_vblank());
        assert!(!c.get_clr_intreq_vblank());

        c.write(0xFF40, 0);
        c.tick(Ticks::from_t(1)).unwrap();
        assert!(!c.get_clr_intreq_vblank());

        for _ in 0..(LCDController::DOTS_PER_LINE * LCDController::SCANLINES) {
            c.tick(Ticks::from_t(1)).unwrap();
        }
        assert!(!c.get_clr_intreq_vblank());
    }

    fn test_cram(xcps_addr: u16, xcpd_addr: u16, lcd: &mut LCDController) {
        macro_rules! read_cram {
            ($entry:expr) => {
                match xcps_addr {
                    0xFF68 => lcd.cram_bg[$entry],
                    0xFF6A => lcd.cram_obj[$entry],
                    _ => unreachable!(),
                }
            };
        }

        // Test writes, byte order
        lcd.write(xcps_addr, 0);
        lcd.write(xcpd_addr, 0xBB);
        lcd.write(xcps_addr, 1);
        lcd.write(xcpd_addr, 0x7A);
        assert_eq!(read_cram!(0), 0x7ABB);

        // Test auto increment
        lcd.write(xcps_addr, 0x20 | XCPS_AUTO_INC);
        assert_eq!(lcd.read(xcps_addr), 0x20 | XCPS_AUTO_INC);
        lcd.write(xcpd_addr, 0x04);
        assert_eq!(lcd.read(xcps_addr), 0x21 | XCPS_AUTO_INC);
        lcd.write(xcpd_addr, 0x03);
        assert_eq!(read_cram!(0x10), 0x0304);
        assert_eq!(lcd.read(xcps_addr), 0x22 | XCPS_AUTO_INC);

        // Test auto increment overflow
        lcd.write(xcps_addr, 0x3F | XCPS_AUTO_INC);
        assert_eq!(lcd.read(xcps_addr), 0x3F | XCPS_AUTO_INC);
        lcd.write(xcpd_addr, 0x55);
        assert_eq!(lcd.read(xcps_addr), 0x00 | XCPS_AUTO_INC);
        lcd.write(xcpd_addr, 0x66);
        assert_eq!(read_cram!(0x3F >> 1) & 0xFF00, 0x5500);
        assert_eq!(read_cram!(0x00) & 0x00FF, 0x0066);

        // Test address mask
        lcd.write(xcps_addr, 0x40 | XCPS_AUTO_INC);
        assert_eq!(lcd.read(xcps_addr), 0x00 | XCPS_AUTO_INC);
        lcd.write(xcpd_addr, 0x0B);
        lcd.write(xcpd_addr, 0x0A);
        assert_eq!(read_cram!(0), 0x0A0B);
    }

    #[test]
    fn cram_bg() {
        let mut c = lcd_cgb();
        test_cram(0xFF68, 0xFF69, &mut c);
    }

    #[test]
    fn cram_obj() {
        let mut c = lcd_cgb();
        test_cram(0xFF6A, 0xFF6B, &mut c);
    }

    #[test]
    fn vram_bank_switching() {
        let mut c = lcd_cgb();

        c.write(0x8000, 0xAA);
        assert_eq!(c.vram[0], 0xAA);
        assert_eq!(c.read(0x8000), 0xAA);
        assert_ne!(c.vram[VRAM_SIZE], 0xAA);
        c.write(0xFF4F, 1);
        assert_ne!(c.read(0x8000), 0xAA);
        c.write(0x8000, 0xBB);
        assert_eq!(c.vram[0], 0xAA);
        assert_eq!(c.read(0x8000), 0xBB);
        assert_eq!(c.vram[VRAM_SIZE], 0xBB);
    }
}
