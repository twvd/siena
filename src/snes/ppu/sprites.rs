use super::*;

pub const OAM_ENTRIES: usize = 128;
pub const TILE_WIDTH: usize = 8;
pub const TILE_HEIGHT: usize = 8;

#[derive(Debug)]
pub struct OAMEntry {
    pub x: usize,
    pub y: usize,
    pub priority: u8,
    pub attr: u8,
    pub tileidx: u16,
    pub width: usize,
    pub height: usize,
}
impl OAMEntry {
    pub fn flip_x(&self) -> bool {
        self.attr & (1 << 6) != 0
    }
    pub fn flip_y(&self) -> bool {
        self.attr & (1 << 7) != 0
    }
    pub fn palette(&self) -> u8 {
        (self.attr >> 1) & 0x07
    }
    pub fn get_tileidx(&self, tile_x: usize, tile_y: usize) -> usize {
        debug_assert!(tile_x < self.width / TILE_WIDTH);
        debug_assert!(tile_y < self.height / TILE_HEIGHT);

        let t_w = self.width / TILE_WIDTH;
        let t_h = self.height / TILE_HEIGHT;

        // Flip tiles as well as the pixels in the tiles
        let tile_x = if self.flip_x() {
            t_w - tile_x - 1
        } else {
            tile_x
        };
        let tile_y = if self.flip_y() {
            t_h - tile_y - 1
        } else {
            tile_y
        };

        (usize::from(self.tileidx) + tile_x + 0x10 * tile_y) & 0x1FF
    }
}

#[derive(Debug)]
pub struct SpriteTile<'a> {
    pub data: &'a [VramWord],
    pub oam: &'a OAMEntry,
}
impl<'a> SpriteTile<'a> {
    pub fn get_coloridx(&self, x: usize, y: usize) -> u8 {
        let mut result: u8 = 0;
        let bitp_w = BPP::Four.num_bitplanes() / VRAM_WORDSIZE;
        let y = if self.oam.flip_y() {
            TILE_HEIGHT - 1 - y
        } else {
            y
        };

        let (x_a, x_b) = if self.oam.flip_x() {
            (1 << x, 1 << 8 + x)
        } else {
            (1 << 7 - x, 1 << 15 - x)
        };

        for i in 0..bitp_w {
            let offset = y + (TILE_WIDTH * i);

            if self.data[offset] & x_a != 0 {
                result |= 1 << (i * VRAM_WORDSIZE);
            }
            if self.data[offset] & x_b != 0 {
                result |= 1 << ((i * VRAM_WORDSIZE) + 1);
            }
        }
        result
    }
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn get_oam_entry(&self, idx: usize) -> OAMEntry {
        let e = &self.oam[(idx * 4)..((idx + 1) * 4)];
        let extoffset_byte = 512 + (idx / 4);
        let extoffset_sh = (idx % 4) * 2;
        let ext = (self.oam[extoffset_byte] >> extoffset_sh) & 0x03;
        let large = ext & 0x02 != 0;
        let (width, height) = match ((self.obsel >> 5) & 0x07, large) {
            // Small sprites
            (0..=2, false) => (8, 8),
            (3..=4, false) => (16, 16),
            (5, false) => (32, 32),
            (6..=7, false) => (16, 32),

            // Large sprites
            (0, true) => (16, 16),
            (1, true) => (32, 32),
            (2 | 4..=5, true) => (64, 64),
            (3 | 7, true) => (32, 32),
            (6, true) => (32, 64),

            _ => unreachable!(),
        };

        OAMEntry {
            x: usize::from(e[0]) | (usize::from(ext) & 0x01) << 8,
            y: e[1].into(),
            tileidx: e[2] as u16 | (e[3] as u16 & 0x01) << 8,
            attr: e[3],
            priority: (e[3] >> 4) & 0x03,
            width,
            height,
        }
    }

    pub fn get_sprite_tile<'a>(
        &'a self,
        oam: &'a OAMEntry,
        tile_x: usize,
        tile_y: usize,
    ) -> SpriteTile {
        let base_addr = (self.obsel as usize & 0x07) * 8192;
        let len = 32 * BPP::Four.num_bitplanes() / VRAM_WORDSIZE;

        let idx = base_addr + (oam.get_tileidx(tile_x, tile_y) << 4);
        let idx = if oam.tileidx >= 0x100 {
            idx + ((self.obsel as usize >> 3) & 3) * 4096
        } else {
            idx
        };
        let idx = idx & VRAM_ADDRMASK;
        let end_idx = (idx + len) & VRAM_ADDRMASK;

        SpriteTile {
            data: &self.vram[idx..end_idx],
            oam,
        }
    }
}
