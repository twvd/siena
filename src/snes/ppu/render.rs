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

    pub fn render_scanline_bglayer(
        &mut self,
        scanline: usize,
        bg: usize,
        line_idx: &mut [u8],
        line_paletted: &mut [Color],
    ) {
        for x in (0..(8 * 32)).step_by(8) {
            // TODO scrolling
            let entry = self.get_tilemap_entry_xy(bg, x, scanline);
            let chr = self.get_tile(bg, &entry);
            let ty = scanline % 8;

            for tx in 0..8 {
                let c = chr.get_coloridx(tx, ty);
                if c == 0 {
                    continue;
                }
                let palette = bg as u8 * 32 + entry.palettenr() * 4;
                let color = self.cgram_to_color(palette + c);
                line_idx[x + tx] = c;
                line_paletted[x + tx] = color;
            }
        }
    }

    pub fn render_scanline(&mut self, scanline: usize) {
        let mut line_idx: [u8; SCREEN_WIDTH] = [0; SCREEN_WIDTH];
        let mut line_paletted: [Color; SCREEN_WIDTH] = [(0, 0, 0); SCREEN_WIDTH];

        match self.get_screen_mode() {
            0 => {
                // 4 layers, 4bpp
                for layer in 0..4 {
                    if self.tm & (1 << layer) == 0 {
                        continue;
                    }

                    self.render_scanline_bglayer(
                        scanline,
                        layer,
                        &mut line_idx,
                        &mut line_paletted,
                    );
                }
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
