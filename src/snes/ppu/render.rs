use super::sprites::{SpriteTile, OAM_ENTRIES};
use super::*;
use crate::frontend::{Color, Renderer};

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn cgram_to_color(&self, addr: u8) -> Color {
        let entry = self.cgram[addr as usize];
        let brightness = (self.inidisp & 0x0F) as usize;

        if brightness == 0 || self.inidisp & 0x80 != 0 {
            // Force blank or no brightness
            return (0, 0, 0);
        }

        // RGB555 -> RGB888 conversion
        let (r, g, b) = (
            (((entry >> 0) & 0x1F) as u8) << 3,  // Red, 5-bit
            (((entry >> 5) & 0x1F) as u8) << 3,  // Green, 5-bit
            (((entry >> 10) & 0x1F) as u8) << 3, // Blue, 5-bit
        );

        // Apply master brightness
        let brightness = brightness + 1;
        let (r, g, b) = (
            ((r as usize * brightness) >> 4) as u8,
            ((g as usize * brightness) >> 4) as u8,
            ((b as usize * brightness) >> 4) as u8,
        );

        (r, g, b)
    }

    fn cindex_to_color<'a>(&self, bg: usize, tile: &impl Tile<'a>, idx: u8) -> Color {
        let paletteidx = tile.get_tile_palette();
        let palette = match tile.get_tile_bpp() {
            BPP::Two if self.get_screen_mode() == 0 => bg as u8 * 32 + paletteidx * 4,
            BPP::Two => paletteidx * 4,
            BPP::Four => paletteidx * 16,
            BPP::Eight => 0,
        };
        self.cgram_to_color(palette + idx)
    }

    fn sprite_cindex_to_color(&self, tile: &SpriteTile, idx: u8) -> Color {
        let palette = 128 + (tile.oam.palette() * 16);
        self.cgram_to_color(palette + idx)
    }

    pub fn render_scanline_bglayer(
        &mut self,
        scanline: usize,
        bg: usize,
        line_idx: &mut [u8],
        line_paletted: &mut [Color],
        priority: bool,
    ) {
        if ((self.tm | self.ts) & !self.dbg_layermask) & (1 << bg) == 0 {
            return;
        }

        let bghofs = self.bgxhofs[bg] as usize;
        let bgvofs = self.bgxvofs[bg] as usize;
        let tilesize = 8;

        for x in 0..SCREEN_WIDTH {
            let entry = self.get_tilemap_entry_xy(bg, x, scanline);
            if entry.bgprio() != priority {
                continue;
            }

            let chr = self.get_bg_tile(bg, &entry);
            let tx = (x + bghofs) % tilesize;
            let ty = (scanline + bgvofs) % tilesize;

            let c = chr.get_coloridx(tx, ty);
            if c == 0 || line_idx[x] != 0 {
                continue;
            }
            line_idx[x] = c;
            line_paletted[x] = self.cindex_to_color(bg, &chr, c);
        }
    }

    pub fn render_scanline_sprites(
        &mut self,
        scanline: usize,
        line_idx: &mut [u8],
        line_paletted: &mut [Color],
        priority: u8,
    ) {
        if ((self.tm | self.ts) & !self.dbg_layermask) & (1 << 4) == 0 {
            return;
        }

        for idx in 0..OAM_ENTRIES {
            let e = self.get_oam_entry(idx);

            if e.priority != priority {
                continue;
            }

            if (e.y..(e.y + e.height)).contains(&scanline) {
                for x in e.x..(e.x + e.width) {
                    if x >= line_idx.len() {
                        break;
                    }

                    let t_x = (x - e.x) / 8;
                    let t_y = (scanline - e.y) / 8;
                    let sprite = self.get_sprite_tile(&e, t_x, t_y);

                    let coloridx = sprite.get_coloridx((x - e.x) % 8, (scanline - e.y) % 8);
                    if coloridx == 0 || line_idx[x] != 0 {
                        continue;
                    }
                    line_idx[x] = coloridx;
                    line_paletted[x] = self.sprite_cindex_to_color(&sprite, coloridx);
                }
            }
        }
    }

    pub fn render_scanline(&mut self, scanline: usize) {
        let mut line_idx: [u8; SCREEN_WIDTH] = [0; SCREEN_WIDTH];
        let mut line_paletted: [Color; SCREEN_WIDTH] = [(0, 0, 0); SCREEN_WIDTH];

        match self.get_screen_mode() {
            0 => {
                // 4 layers, 2bpp (4 colors)
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut line_idx, &mut line_paletted, true);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut line_idx, &mut line_paletted, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 2);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut line_idx, &mut line_paletted, false);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut line_idx, &mut line_paletted, false);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 1);
                // BG3 tiles with priority 1
                self.render_scanline_bglayer(scanline, 2, &mut line_idx, &mut line_paletted, true);
                // BG4 tiles with priority 1
                self.render_scanline_bglayer(scanline, 3, &mut line_idx, &mut line_paletted, true);
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 0);
                // BG3 tiles with priority 0
                self.render_scanline_bglayer(scanline, 2, &mut line_idx, &mut line_paletted, false);
                // BG4 tiles with priority 0
                self.render_scanline_bglayer(scanline, 3, &mut line_idx, &mut line_paletted, false);
            }
            1 => {
                let bg3_prio = self.bgmode & (1 << 3) != 0;
                // BG3 tiles with priority 1 if bit 3 of $2105 is set
                if bg3_prio {
                    self.render_scanline_bglayer(
                        scanline,
                        2,
                        &mut line_idx,
                        &mut line_paletted,
                        true,
                    );
                }
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut line_idx, &mut line_paletted, true);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut line_idx, &mut line_paletted, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 2);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut line_idx, &mut line_paletted, false);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut line_idx, &mut line_paletted, false);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 1);
                // BG3 tiles with priority 1 if bit 3 of $2105 is clear
                if !bg3_prio {
                    self.render_scanline_bglayer(
                        scanline,
                        2,
                        &mut line_idx,
                        &mut line_paletted,
                        true,
                    );
                }
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 0);
                // BG3 tiles with priority 0
                self.render_scanline_bglayer(scanline, 2, &mut line_idx, &mut line_paletted, false);
            }
            3 => {
                // 2 layers, bg1: 8bpp (256 colors)
                // bg2: 4bpp (16 colors)
                // Sprites with priority 3
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 3);
                // BG1 tiles with priority 1
                self.render_scanline_bglayer(scanline, 0, &mut line_idx, &mut line_paletted, true);
                // Sprites with priority 2
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 2);
                // BG2 tiles with priority 1
                self.render_scanline_bglayer(scanline, 1, &mut line_idx, &mut line_paletted, true);
                // Sprites with priority 1
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 1);
                // BG1 tiles with priority 0
                self.render_scanline_bglayer(scanline, 0, &mut line_idx, &mut line_paletted, false);
                // Sprites with priority 0
                self.render_scanline_sprites(scanline, &mut line_idx, &mut line_paletted, 0);
                // BG2 tiles with priority 0
                self.render_scanline_bglayer(scanline, 1, &mut line_idx, &mut line_paletted, false);
            }
            _ => todo!(),
        }

        for (x, p) in line_paletted.into_iter().enumerate() {
            if line_idx[x] == 0 {
                // Use main backdrop color
                self.renderer.set_pixel(x, scanline, self.cgram_to_color(0));
            } else {
                self.renderer.set_pixel(x, scanline, p);
            }
        }
    }
}
