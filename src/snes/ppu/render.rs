use super::color::SnesColor;
use super::sprites::{SpriteTile, OAM_ENTRIES};
use super::*;
use crate::frontend::Renderer;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

const LAYER_BACKDROP: u8 = 255;
const LAYER_SPRITES: u8 = 4;
pub struct RenderState {
    /// Color index from tile data
    pub idx: [u8; SCREEN_WIDTH],

    /// Palette from OAM (sprites only!)
    pub palette: [u8; SCREEN_WIDTH],

    /// Paletted color
    pub paletted: [SnesColor; SCREEN_WIDTH],

    /// Layer that produced the pixel
    pub layer: [u8; SCREEN_WIDTH],

    /// Layer mask for the window
    pub windowlayermask: u8,

    /// Layer mask
    pub layermask: u8,

    /// State/settings of the window
    pub window: WindowState,
}

impl RenderState {
    pub fn new(
        backdrop: SnesColor,
        layermask: u8,
        window: WindowState,
        windowlayermask: u8,
    ) -> Self {
        Self {
            idx: [0; SCREEN_WIDTH],
            palette: [0; SCREEN_WIDTH],
            paletted: [backdrop; SCREEN_WIDTH],
            layer: [LAYER_BACKDROP; SCREEN_WIDTH],
            layermask,
            window,
            windowlayermask,
        }
    }
}

type WindowLine = [bool; SCREEN_WIDTH];

#[derive(Clone)]
pub struct WindowState {
    bg: [WindowLine; 4],
    math: WindowLine,
    sprites: WindowLine,
}

const WINAREA_OUTER: u8 = 1 << 0;
const WINAREA_ENABLE: u8 = 1 << 1;
const WINAREA_MASK: u8 = 0x03;

#[derive(FromPrimitive, Eq, PartialEq, Copy, Clone, Debug)]
enum WindowMask {
    Or = 0,
    And = 1,
    Xor = 2,
    Xnor = 3,
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn cgram_to_color(&self, addr: u8) -> SnesColor {
        SnesColor::from(self.cgram[addr as usize])
    }

    fn cindex_to_color<'a>(&self, bg: usize, tile: &impl Tile<'a>, idx: u8) -> SnesColor {
        let paletteidx = tile.get_tile_palette();
        let palette = match tile.get_tile_bpp() {
            BPP::Two if self.get_screen_mode() == 0 => bg as u8 * 32 + paletteidx * 4,
            BPP::Two => paletteidx * 4,
            BPP::Four => paletteidx * 16,
            BPP::Eight => 0,
        };
        self.cgram_to_color(palette + idx)
    }

    fn sprite_cindex_to_color(&self, tile: &SpriteTile, idx: u8) -> SnesColor {
        let palette = 128 + (tile.oam.palette() * 16);
        self.cgram_to_color(palette + idx)
    }

    fn render_scanline_bglayer(
        &mut self,
        scanline: usize,
        bg: usize,
        state: &mut RenderState,
        priority: bool,
    ) {
        if state.layermask & (1 << bg) == 0 {
            return;
        }

        let bghofs = self.bgxhofs[bg] as usize;
        let bgvofs = self.bgxvofs[bg] as usize;
        let tilesize = self.get_bg_tile_size(bg);

        for x in 0..SCREEN_WIDTH {
            let entry = self.get_tilemap_entry_xy(bg, x, scanline);
            if entry.bgprio() != priority {
                continue;
            }

            if state.window.bg[bg][x] && state.windowlayermask & (1 << bg) != 0 {
                // Masked by window.
                continue;
            }

            // Determine coordinates within the tile. This is
            // a full tile (so either 8x8 or 16x16).
            let px_x = (x + bghofs) % tilesize;
            let px_y = (scanline + bgvofs) % tilesize;
            // get_bg_tile will select the sub-tile (for 16x16).
            let tile = self.get_bg_tile(bg, &entry, px_x, px_y);

            // Wrap coordinates back here to the (sub)-tile size
            let c = tile.get_coloridx(px_x % TILE_WIDTH, px_y % TILE_HEIGHT);
            if c == 0 || state.idx[x] != 0 {
                continue;
            }
            state.idx[x] = c;
            state.paletted[x] = self.cindex_to_color(bg, &tile, c);
            state.layer[x] = bg as u8;
        }
    }

    fn render_scanline_sprites(&mut self, scanline: usize, state: &mut RenderState, priority: u8) {
        if state.layermask & (1 << LAYER_SPRITES) == 0 {
            return;
        }

        // Sprites are actually offset 1 scanline down, because on the original hardware
        // the sprites for the NEXT scanline are fetched during the CURRENT scanline.
        // On scanline 0, the sprites at Y 0 are fetched to be drawn at scanline 1.
        // This is also why scanline 0 is never rendered.
        let scanline = scanline - 1;

        for idx in 0..OAM_ENTRIES {
            let prio_idx = if self.oam_priority {
                // Priority rotation enabled
                (idx + self.oamadd_reload.get() as usize) % OAM_ENTRIES
            } else {
                idx
            };
            let e = self.get_oam_entry(prio_idx);

            if e.priority != priority {
                continue;
            }

            if (e.y..(e.y + e.height)).contains(&scanline) {
                for x in e.x..(e.x + e.width) {
                    if x >= state.idx.len() {
                        // Outside of visible area.
                        break;
                    }
                    if state.window.sprites[x] && state.windowlayermask & (1 << LAYER_SPRITES) != 0
                    {
                        // Masked by window.
                        continue;
                    }

                    let t_x = (x - e.x) / TILE_WIDTH;
                    let t_y = (scanline - e.y) / TILE_HEIGHT;
                    let sprite = self.get_sprite_tile(&e, t_x, t_y);

                    let coloridx =
                        sprite.get_coloridx((x - e.x) % TILE_WIDTH, (scanline - e.y) % TILE_HEIGHT);
                    if coloridx == 0 || state.idx[x] != 0 {
                        continue;
                    }
                    state.idx[x] = coloridx;
                    state.palette[x] = sprite.oam.palette();
                    state.paletted[x] = self.sprite_cindex_to_color(&sprite, coloridx);
                    state.layer[x] = LAYER_SPRITES;
                }
            }
        }
    }

    fn render_scanline_screen(
        &mut self,
        scanline: usize,
        layermask: u8,
        backdrop: SnesColor,
        windows: WindowState,
        windowlayermask: u8,
    ) -> RenderState {
        // TODO remove double copies of WindowState
        let mut state = RenderState::new(backdrop, layermask, windows, windowlayermask);

        match self.get_screen_mode() {
            0 => {
                // 4 layers, 2bpp (4 colors)
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut state, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut state, true);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut state, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut state, 2);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut state, false);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut state, false);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut state, 1);
                // BG3 tiles with priority 1
                self.render_scanline_bglayer(scanline, 2, &mut state, true);
                // BG4 tiles with priority 1
                self.render_scanline_bglayer(scanline, 3, &mut state, true);
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut state, 0);
                // BG3 tiles with priority 0
                self.render_scanline_bglayer(scanline, 2, &mut state, false);
                // BG4 tiles with priority 0
                self.render_scanline_bglayer(scanline, 3, &mut state, false);
            }
            1 => {
                let bg3_prio = self.bgmode & (1 << 3) != 0;
                // BG3 tiles with priority 1 if bit 3 of $2105 is set
                if bg3_prio {
                    self.render_scanline_bglayer(scanline, 2, &mut state, true);
                }
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut state, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut state, true);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut state, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut state, 2);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut state, false);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut state, false);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut state, 1);
                // BG3 tiles with priority 1 if bit 3 of $2105 is clear
                if !bg3_prio {
                    self.render_scanline_bglayer(scanline, 2, &mut state, true);
                }
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut state, 0);
                // BG3 tiles with priority 0
                self.render_scanline_bglayer(scanline, 2, &mut state, false);
            }
            2 => {
                // 2 layers, bg1: 4bpp (16 colors), bg2: 4bpp (16 colors)
                // bg3: Offset-per-tile
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut state, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut state, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut state, 2);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut state, true);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut state, 1);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut state, false);
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut state, 0);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut state, false);
            }
            3 => {
                // 2 layers, bg1: 8bpp (256 colors)
                // bg2: 4bpp (16 colors)
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut state, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut state, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut state, 2);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut state, true);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut state, 1);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut state, false);
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut state, 0);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut state, false);
            }
            7 => {
                // TODO extbg
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut state, 3);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut state, 2);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut state, 1);
                // BG1
                self.render_scanline_mode7(scanline, &mut state);
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut state, 0);
            }
            _ => println!(
                "TODO unimplemented PPU mode {} at scanline {}",
                self.get_screen_mode(),
                scanline
            ),
        }

        state
    }

    pub fn render_scanline(&mut self, scanline: usize, output_offset: isize) {
        let windows = self.render_windows();
        let mainscreen = self.render_scanline_screen(
            scanline,
            self.tm & !self.dbg_layermask,
            self.cgram_to_color(0),
            windows.clone(),
            self.tmw,
        );
        let subscreen = self.render_scanline_screen(
            scanline,
            if self.cgwsel & (1 << 1) != 0 {
                // Enable backdrop + bg + obj
                self.ts & !self.dbg_layermask
            } else {
                // Backdrop only
                0
            },
            self.coldata,
            windows.clone(),
            self.tsw,
        );

        // Send line to screen buffer
        let output_line = usize::try_from((scanline as isize) + output_offset).unwrap();
        let brightness = (self.inidisp & 0x0F) as usize;

        for x in 0..mainscreen.paletted.len() {
            if brightness == 0 || self.inidisp & 0x80 != 0 {
                // Force blank or no brightness
                let renderer = self.renderer.as_mut().unwrap();
                renderer.set_pixel(x, output_line, SnesColor::BLACK.to_native());
                continue;
            }

            let pixel = self.apply_colormath(
                mainscreen.paletted[x],
                subscreen.paletted[x],
                mainscreen.layer[x],
                subscreen.layer[x],
                mainscreen.window.math[x],
                mainscreen.palette[x],
            );

            // Apply master brightness and output
            let renderer = self.renderer.as_mut().unwrap();
            renderer.set_pixel(
                x,
                output_line,
                pixel.apply_brightness(brightness).to_native(),
            );
        }
    }

    fn apply_colormath(
        &self,
        mainclr: SnesColor,
        subclr: SnesColor,
        mainlayer: u8,
        sublayer: u8,
        in_window: bool,
        mainpalette: u8,
    ) -> SnesColor {
        // 5-4  Color Math Enable
        // (0=Always, 1=MathWindow, 2=NotMathWin, 3=Never)
        let cm_enable = (self.cgwsel >> 4) & 0x03;
        // 7-6  Force Main Screen Black
        // (3=Always, 2=MathWindow, 1=NotMathWin, 0=Never)
        let force_main_black = (self.cgwsel >> 6) & 0x03;

        let mut pixel = mainclr;

        if force_main_black == 3
            || (in_window && force_main_black == 2)
            || (!in_window && force_main_black == 1)
        {
            pixel = SnesColor::BLACK;
        }

        let apply = 0
            != self.cgadsub
                & (1 << match mainlayer {
                    LAYER_BACKDROP => 5,
                    // BG1/BG2/BG3/BG4/OBJ
                    _ => mainlayer,
                });
        if !apply || cm_enable == 3 {
            // Disabled always or disabled for layer
            return pixel;
        }

        if mainlayer == LAYER_SPRITES && (0..4).contains(&mainpalette) {
            // Sprite palette 0-3 are unaffected by color math
            return pixel;
        }

        if (cm_enable == 1 && !in_window) || (cm_enable == 2 && in_window) {
            // In wrong side of window
            return pixel;
        }

        let div2 = self.cgadsub & (1 << 6) != 0 && sublayer != LAYER_BACKDROP;
        if self.cgadsub & (1 << 7) == 0 {
            // Add mode
            pixel = pixel.cm_add(&subclr, div2);
        } else {
            // Subtract mode
            pixel = pixel.cm_sub(&subclr, div2);
        }

        pixel
    }

    fn render_window(&self, w1area: u8, w2area: u8, mask: WindowMask) -> WindowLine {
        let in_window = |area, range: &std::ops::RangeInclusive<u8>, x| {
            let is_outer = (area & WINAREA_OUTER) != 0;
            (!is_outer && range.contains(&x)) || (is_outer && !range.contains(&x))
        };

        let w1 = self.w1_left..=self.w1_right;
        let w2 = self.w2_left..=self.w2_right;
        let mut ret = [false; SCREEN_WIDTH];

        if (w1area | w2area) & WINAREA_ENABLE == 0 {
            return ret;
        }

        let use_masking = w1area & w2area & WINAREA_ENABLE != 0;

        for x in 0..=u8::MAX {
            let in_w1 = in_window(w1area, &w1, x);
            let in_w2 = in_window(w2area, &w2, x);

            if !use_masking {
                ret[x as usize] = in_w1 || in_w2;
            } else {
                ret[x as usize] = match mask {
                    WindowMask::Or => in_w1 || in_w2,
                    WindowMask::And => in_w1 && in_w2,
                    WindowMask::Xor => in_w1 ^ in_w2,
                    WindowMask::Xnor => !(in_w1 ^ in_w2),
                };
            }
        }

        ret
    }

    fn render_windows(&self) -> WindowState {
        // Bit  W12   W34   WOBJ
        // 7-6  BG2   BG4   MATH  Window-2 Area
        // 5-4  BG2   BG4   MATH  Window-1 Area
        // 3-2  BG1   BG3   OBJ   Window-2 Area
        // 1-0  BG1   BG3   OBJ   Window-1 Area
        //
        // Bit  WBG   WOBJ
        // 7-6  BG4   -     Window 1/2 Mask Logic
        // 5-4  BG3   -     Window 1/2 Mask Logic
        // 3-2  BG2   MATH  Window 1/2 Mask Logic
        // 1-0  BG1   OBJ   Window 1/2 Mask Logic
        let w12a = |sh| (self.w12sel >> sh) & WINAREA_MASK;
        let w34a = |sh| (self.w34sel >> sh) & WINAREA_MASK;
        let wobja = |sh| (self.wobjsel >> sh) & WINAREA_MASK;
        let wbgm = |sh| WindowMask::from_u8((self.wbglog >> sh) & 0x03_u8).unwrap();
        let wobjm = |sh| WindowMask::from_u8((self.wobjlog >> sh) & 0x03_u8).unwrap();

        WindowState {
            bg: [
                self.render_window(w12a(0), w12a(2), wbgm(0)),
                self.render_window(w12a(4), w12a(6), wbgm(2)),
                self.render_window(w34a(0), w34a(2), wbgm(4)),
                self.render_window(w34a(4), w34a(6), wbgm(6)),
            ],
            sprites: self.render_window(wobja(0), wobja(2), wobjm(0)),
            math: self.render_window(wobja(4), wobja(6), wobjm(2)),
        }
    }
}
