use crate::tickable::Ticks;

use serde::{Deserialize, Serialize};

pub const APU_TIMERS: usize = 3;

/// One APU timer
#[derive(Serialize, Deserialize)]
pub struct Timer {
    /// The top register
    top: usize,

    /// Internal tick counter
    ticks: usize,

    /// Counter register (4-bit)
    cnt: u8,

    /// Divider
    div: Ticks,
}

impl Timer {
    pub fn new(div: Ticks) -> Self {
        Self {
            top: 0,
            ticks: 0,
            cnt: 0,
            div,
        }
    }

    pub fn reset(&mut self) {
        self.ticks = 0;
        self.cnt = 0;
    }

    pub fn tick(&mut self, ticks: Ticks) {
        // This is supposed to be called at the APU frequency,
        // the timer will scale down by its divider.

        self.ticks += ticks;
        if self.ticks / self.div >= usize::from(self.top) {
            self.cnt = (self.cnt + 1) & 0x0F;
            self.ticks -= self.top * self.div;
        }
    }

    pub fn set_top(&mut self, top: u8) {
        self.top = usize::from(top)
    }

    pub fn get_cnt(&self) -> u8 {
        self.cnt
    }
}
