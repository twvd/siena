use super::PPU;
use crate::frontend::Renderer;

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn render_scanline(&mut self, scanline: usize) {
        for x in 0..340 {
            self.renderer.set_pixel(x, scanline, (scanline as u8, 0, 0));
        }
    }
}
