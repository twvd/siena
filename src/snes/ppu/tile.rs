use super::*;

/// A tile is a single 8 x 8 pixel segment of a background layer
/// or a sprite.
pub trait Tile<'tdata> {
    fn get_tile_data(&self) -> &'tdata [VramWord];
    fn get_tile_flip_x(&self) -> bool;
    fn get_tile_flip_y(&self) -> bool;
    fn get_tile_bpp(&self) -> BPP;
    fn get_tile_palette(&self) -> u8;

    /// Get a single pixel (color index) of a tile.
    fn get_coloridx(&self, x: usize, y: usize) -> u8 {
        let mut result: u8 = 0;
        let bitp_w = self.get_tile_bpp().num_bitplanes() / VRAM_WORDSIZE;
        let y = if self.get_tile_flip_y() { 7 - y } else { y };

        let (x_a, x_b) = if self.get_tile_flip_x() {
            (1 << x, 1 << 8 + x)
        } else {
            (1 << 7 - x, 1 << 15 - x)
        };

        let data = self.get_tile_data();
        for i in 0..bitp_w {
            let offset = y + (8 * i);

            if data[offset] & x_a != 0 {
                result |= 1 << (i * VRAM_WORDSIZE);
            }
            if data[offset] & x_b != 0 {
                result |= 1 << ((i * VRAM_WORDSIZE) + 1);
            }
        }
        result
    }
}
