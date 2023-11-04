use super::*;
use crate::frontend::{Color, Renderer};

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn cgram_to_color(&self, addr: u8) -> Color {
        let entry = self.cgram[addr as usize];
        (
            (((entry >> 0) & 0x1F) as u8) << 3,  // Red, 5-bit
            (((entry >> 5) & 0x1F) as u8) << 3,  // Green, 5-bit
            (((entry >> 10) & 0x1F) as u8) << 3, // Blue, 5-bit
        )
    }

    fn cindex_to_color(&self, bg: usize, tile: &Tile, idx: u8) -> Color {
        let palette = match tile.bpp {
            BPP::Two => bg as u8 * 32 + tile.map.palettenr() * 4,
            BPP::Four => tile.map.palettenr() * 16,
            BPP::Eight => 0,
        };
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
        if self.tm & (1 << bg) == 0 {
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

            let chr = self.get_tile(bg, &entry);
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

    pub fn render_scanline(&mut self, scanline: usize) {
        let mut line_idx: [u8; SCREEN_WIDTH] = [0; SCREEN_WIDTH];
        let mut line_paletted: [Color; SCREEN_WIDTH] = [(0, 0, 0); SCREEN_WIDTH];

        match self.get_screen_mode() {
            0 => {
                let mut rs = |layer, prio| {
                    self.render_scanline_bglayer(
                        scanline,
                        layer,
                        &mut line_idx,
                        &mut line_paletted,
                        prio,
                    )
                };
                // 4 layers, 2bpp (4 colors)
                // TODO Sprites with priority 3
                // BG1 tiles with priority 1
                rs(0, true);
                // BG2 tiles with priority 1
                rs(1, true);
                // TODO Sprites with priority 2
                // BG1 tiles with priority 0
                rs(0, false);
                // BG2 tiles with priority 0
                rs(1, false);
                // TODO Sprites with priority 1
                // BG3 tiles with priority 1
                rs(2, true);
                // BG4 tiles with priority 1
                rs(3, true);
                // TODO Sprites with priority 0
                // BG3 tiles with priority 0
                rs(2, false);
                // BG4 tiles with priority 0
                rs(3, false);
            }
            3 => {
                let mut rs = |layer, prio| {
                    self.render_scanline_bglayer(
                        scanline,
                        layer,
                        &mut line_idx,
                        &mut line_paletted,
                        prio,
                    )
                };
                // 2 layers, bg1: 8bpp (256 colors)
                // bg2: 4bpp (16 colors)
                // TODO Sprites with priority 3
                // BG1 tiles with priority 1
                rs(0, true);
                // TODO Sprites with priority 2
                // BG2 tiles with priority 1
                rs(1, true);
                // TODO Sprites with priority 1
                // BG1 tiles with priority 0
                rs(0, false);
                // TODO Sprites with priority 0
                // BG2 tiles with priority 0
                rs(1, false);
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
