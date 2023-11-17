use super::color::SnesColor;
use super::sprites::{SpriteTile, OAM_ENTRIES};
use super::*;
use crate::frontend::Renderer;

const LAYER_BACKDROP: u8 = 255;
struct RenderState {
    idx: [u8; SCREEN_WIDTH],
    paletted: [SnesColor; SCREEN_WIDTH],
    layer: [u8; SCREEN_WIDTH],
    layermask: u8,
}

impl RenderState {
    pub fn new(backdrop: SnesColor, layermask: u8) -> Self {
        Self {
            idx: [0; SCREEN_WIDTH],
            paletted: [backdrop; SCREEN_WIDTH],
            layer: [LAYER_BACKDROP; SCREEN_WIDTH],
            layermask,
        }
    }
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn cgram_to_color(&self, addr: u8) -> SnesColor {
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
        if state.layermask & (1 << 4) == 0 {
            return;
        }

        for idx in 0..OAM_ENTRIES {
            let e = self.get_oam_entry(idx);

            if e.priority != priority {
                continue;
            }

            if (e.y..(e.y + e.height)).contains(&scanline) {
                for x in e.x..(e.x + e.width) {
                    if x >= state.idx.len() {
                        break;
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
                    state.paletted[x] = self.sprite_cindex_to_color(&sprite, coloridx);
                    state.layer[x] = 4;
                }
            }
        }
    }

    fn render_scanline_screen(
        &mut self,
        scanline: usize,
        layermask: u8,
        backdrop: SnesColor,
    ) -> RenderState {
        let mut state = RenderState::new(backdrop, layermask);

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
            _ => todo!(),
        }

        state
    }

    pub fn render_scanline(&mut self, scanline: usize) {
        let mainscreen = self.render_scanline_screen(
            scanline,
            self.tm & !self.dbg_layermask,
            self.cgram_to_color(0),
        );
        let subscreen =
            self.render_scanline_screen(scanline, self.ts & !self.dbg_layermask, self.coldata);

        // Send line to screen buffer
        let brightness = (self.inidisp & 0x0F) as usize;
        for x in 0..mainscreen.paletted.len() {
            if brightness == 0 || self.inidisp & 0x80 != 0 {
                // Force blank or no brightness
                self.renderer
                    .set_pixel(x, scanline, SnesColor::BLACK.to_native());
                continue;
            }

            let pixel = self.apply_colormath(
                mainscreen.paletted[x],
                subscreen.paletted[x],
                mainscreen.layer[x],
                subscreen.layer[x],
            );

            // Apply master brightness and output
            self.renderer
                .set_pixel(x, scanline, pixel.apply_brightness(brightness).to_native());
        }
    }

    fn apply_colormath(
        &self,
        mainclr: SnesColor,
        subclr: SnesColor,
        mainlayer: u8,
        sublayer: u8,
    ) -> SnesColor {
        let mut pixel = mainclr;

        let apply = 0
            != self.cgadsub
                & (1 << match mainlayer {
                    LAYER_BACKDROP => 5,
                    // BG1/BG2/BG3/BG4/OBJ
                    _ => mainlayer,
                });
        if !apply || (self.cgwsel >> 4) & 3 == 3 {
            // Disabled
            return pixel;
        }
        // TODO MathWindow

        if self.cgadsub & (1 << 7) == 0 {
            // Add mode
            pixel += subclr;
        } else {
            // Subtract mode
            pixel -= subclr;
        }
        if self.cgadsub & (1 << 6) != 0 {
            // Division by 2
            pixel = pixel.div_2();
        }

        pixel
    }
}
