use super::PPU;
use crate::frontend::Renderer;

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn render_scanline(&mut self, scanline: usize) {
        for x in 0..340 {
            let entry = self.get_tilemap_entry_xy(0, x, scanline);
            if entry.charnr() > 0 {
                self.renderer.set_pixel(x, scanline, (255, 0, 0));
            }
        }
    }
}
