use super::*;
use crate::frontend::{Color, Renderer};

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn render_scanline(&mut self, scanline: usize) {
        let mut line_idx: [u8; SCREEN_WIDTH] = [0; SCREEN_WIDTH];
        let mut line_paletted: [Color; SCREEN_WIDTH] = [(0, 0, 0); SCREEN_WIDTH];

        for x in (0..(8 * 32)).step_by(8) {
            let entry = self.get_tilemap_entry_xy(0, x, scanline);
            let chr = self.get_tile(0, &entry);
            let ty = scanline % 8;

            for tx in 0..8 {
                let c = chr.get_coloridx(tx, ty);
                line_idx[x + tx] = c;
                line_paletted[x + tx] = (70 * c, 70 * c, 70 * c);
            }
        }

        for (x, p) in line_paletted.into_iter().enumerate() {
            self.renderer.set_pixel(x, scanline, p);
        }
    }
}
