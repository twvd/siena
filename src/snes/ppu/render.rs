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
    pub fn render_scanline(&mut self, scanline: usize) {
        let mut line_idx: [u8; SCREEN_WIDTH] = [0; SCREEN_WIDTH];
        let mut line_paletted: [Color; SCREEN_WIDTH] = [(0, 0, 0); SCREEN_WIDTH];

        for x in (0..(8 * 32)).step_by(8) {
            let entry = self.get_tilemap_entry_xy(0, x, scanline);
            let chr = self.get_tile(0, &entry);
            let ty = scanline % 8;

            for tx in 0..8 {
                let palette = entry.palettenr() * 4;
                let c = chr.get_coloridx(tx, ty);
                let color = self.cgram_to_color(palette + c);
                line_idx[x + tx] = c;
                line_paletted[x + tx] = color;
            }
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
