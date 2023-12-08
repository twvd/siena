use super::render::*;
use super::*;
use crate::frontend::Renderer;

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn mode7_vram_to_color(&self, vram_x: i32, vram_y: i32) -> u8 {
        let tile_x = ((vram_x as u32 >> 11) & 0x7f) as u16;
        let tile_y = ((vram_y as u32 >> 11) & 0x7f) as u16;
        let pixel_x = ((vram_x >> 8) & 0x07) as u16;
        let pixel_y = ((vram_y >> 8) & 0x07) as u16;

        let tilemap_addr = ((tile_y << 7) + tile_x) as usize;
        let tileidx = self.vram[tilemap_addr & VRAM_ADDRMASK] & 0xFF;
        let pixel_addr = ((tileidx << 6) + (pixel_y << 3) + pixel_x) as usize;
        (self.vram[pixel_addr & VRAM_ADDRMASK] >> 8) as u8
    }

    fn mode7_get_pixel(&self, x: usize, scanline: usize) -> u8 {
        // IF xflip THEN SCREEN.X=((0..255) XOR FFh), ELSE SCREEN.X=(0..255)
        let screen_x = if self.m7sel & 0x01 != 0 {
            // H-flip
            (x & 0xFF) ^ 0xFF
        } else {
            x & 0xFF
        } as i16;
        // IF yflip THEN SCREEN.Y=((1..224/239) XOR FFh), ELSE SCREEN.Y=(1..224/239)
        let screen_y = if self.m7sel & 0x02 != 0 {
            // V-flip
            (scanline & 0xFF) ^ 0xFF
        } else {
            scanline
        } as i16;
        // ORG.X = (M7HOFS-M7X) AND NOT 1C00h, IF ORG.X<0 THEN ORG.X=ORG.X OR 1C00h
        let mut org_x = (self.m7hofs - self.m7x) & !0x1C00;
        if org_x < 0 {
            org_x |= 0x1C00;
        }

        // ORG.Y = (M7VOFS-M7Y) AND NOT 1C00h, IF ORG.Y<0 THEN ORG.Y=ORG.Y OR 1C00h
        let mut org_y = (self.m7vofs - self.m7y) & !0x1C00;
        if org_y < 0 {
            org_y |= 0x1C00;
        }
        // VRAM.X = ((M7A*ORG.X) AND NOT 3Fh) + ((M7B*ORG.Y) AND NOT 3Fh) + M7X*100h
        let mut vram_x = ((self.m7a as i32 * org_x as i32) & !0x3F)
            + ((self.m7b as i32 * org_y as i32) & !0x3F)
            + self.m7x as i32 * 0x100;
        // VRAM.Y = ((M7C*ORG.X) AND NOT 3Fh) + ((M7D*ORG.Y) AND NOT 3Fh) + M7Y*100h
        let mut vram_y = ((self.m7c as i32 * org_x as i32) & !0x3F)
            + ((self.m7d as i32 * org_y as i32) & !0x3F)
            + self.m7y as i32 * 0x100;
        // VRAM.X = VRAM.X + ((M7B*SCREEN.Y) AND NOT 3Fh) + (M7A*SCREEN.X)
        vram_x +=
            ((self.m7b as i32 * screen_y as i32) & !0x3F) + (self.m7a as i32 * screen_x as i32);
        // VRAM.Y = VRAM.Y + ((M7D*SCREEN.Y) AND NOT 3Fh) + (M7C*SCREEN.X)
        vram_y +=
            ((self.m7d as i32 * screen_y as i32) & !0x3F) + (self.m7c as i32 * screen_x as i32);

        self.mode7_vram_to_color(vram_x, vram_y)
    }

    pub fn render_scanline_mode7(&mut self, scanline: usize, state: &mut RenderState) {
        for x in 0..SCREEN_WIDTH {
            let c = self.mode7_get_pixel(x, scanline);
            if c == 0 || state.idx[x] != 0 {
                continue;
            }
            state.idx[x] = c;
            state.paletted[x] = self.cgram_to_color(c);
            state.layer[x] = 1 as u8;
        }
    }
}