use itertools::Itertools;

const OAM_ENTRY_SIZE: usize = 4;
const OAM_SIZE: usize = 0xA0;
const OAM_ENTRIES: usize = OAM_SIZE / OAM_ENTRY_SIZE;

/// One single table entry
#[derive(Copy, Clone, Debug)]
pub struct OAMEntry {
    pub x: u8,
    pub y: u8,
    pub tile_idx: u8,
    pub flags: u8,
}

impl OAMEntry {
    pub fn new() -> Self {
        Self {
            x: 0,
            y: 0,
            tile_idx: 0,
            flags: 0,
        }
    }

    pub fn read(&self, addr: usize) -> u8 {
        match addr % OAM_ENTRY_SIZE {
            0 => self.y,
            1 => self.x,
            2 => self.tile_idx,
            3 => self.flags,
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, addr: usize, val: u8) {
        match addr % OAM_ENTRY_SIZE {
            0 => self.y = val,
            1 => self.x = val,
            2 => self.tile_idx = val,
            3 => self.flags = val,
            _ => unreachable!(),
        }
    }
}

/// Sprite Attribute Table / Object Attribute Memory
pub struct OAMTable {
    oam: [OAMEntry; OAM_ENTRIES],
}

/// Object Priority Mode
#[derive(Debug, Clone, Copy)]
pub enum ObjPriMode {
    /// By X-coordinate (only option on DMG)
    Coordinate,

    /// By OAM position (default on CGB)
    OAMPosition,
}

impl OAMTable {
    pub fn new() -> Self {
        Self {
            oam: [OAMEntry::new(); OAM_ENTRIES],
        }
    }

    pub fn iter_scanline(
        &self,
        y: isize,
        sprite_h: isize,
        mode: ObjPriMode,
    ) -> Box<dyn Iterator<Item = &OAMEntry> + '_> {
        // TODO remove the heap allocation of the iterator

        let y = y + 16;
        match mode {
            ObjPriMode::Coordinate => Box::new(
                self.oam
                    .iter()
                    // Select objects in current scanline
                    .filter(move |&&e| (e.y as isize) <= y && (e.y as isize + sprite_h) > y)
                    // OAM scan only collects 10 objects per scanline
                    .take(10)
                    // Objects have priority from low X to high. To simplify this,
                    // just sort and draw right to left.
                    .sorted_by_key(|&e| e.x)
                    .rev(),
            ),
            ObjPriMode::OAMPosition => Box::new(
                self.oam
                    .iter()
                    // Select objects in current scanline
                    .filter(move |&&e| (e.y as isize) <= y && (e.y as isize + sprite_h) > y)
                    // OAM scan only collects 10 objects per scanline
                    .take(10)
                    // Draw in opposite order to get overlapping right
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev(),
            ),
        }
    }

    pub fn read(&self, addr: usize) -> u8 {
        self.oam[addr / OAM_ENTRY_SIZE].read(addr % OAM_ENTRY_SIZE)
    }

    pub fn write(&mut self, addr: usize, val: u8) {
        self.oam[addr / OAM_ENTRY_SIZE].write(addr % OAM_ENTRY_SIZE, val)
    }
}
