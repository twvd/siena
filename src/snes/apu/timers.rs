use std::cell::Cell;

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
    cnt: Cell<u8>,

    /// Internal (low) part of counter (4-bits)
    cnt_low: u8,

    /// Divider
    div: Ticks,
}

impl Timer {
    pub fn new(div: Ticks) -> Self {
        Self {
            top: 0,
            ticks: 0,
            cnt: Cell::new(0),
            cnt_low: 0,
            div,
        }
    }

    pub fn reset(&mut self) {
        self.ticks = 0;
        self.cnt.set(0);
    }

    pub fn tick(&mut self, ticks: Ticks) {
        // This is supposed to be called at the APU frequency,
        // the timer will scale down by its divider.

        self.ticks += ticks;
        while self.ticks >= self.div {
            self.ticks -= self.div;
            self.cnt_low += 1;
            if self.top != 0 {
                if self.cnt_low == self.top as u8 {
                    self.cnt.set((self.cnt.get() + 1) & 0x0F);
                    self.cnt_low = 0;
                }
            }
        }
    }

    pub fn set_top(&mut self, top: u8) {
        self.top = usize::from(top)
    }

    pub fn get_cnt(&self) -> u8 {
        let v = self.cnt.get();
        self.cnt.set(0);
        v
    }
}
