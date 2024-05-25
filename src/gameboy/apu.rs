use crate::gameboy::bus::bus::BusMember;
use crate::gameboy::tickable::{Tickable, Ticks};

use anyhow::Result;

/// Amount of APU channels
const APU_CHANNELS: usize = 4;

/// Gameboy Audio Processing Unit
pub struct APU {
    apu_enable: bool,
    dac_enable: u8,
    len_timers: [u8; APU_CHANNELS],
    nr50: u8,
}

impl APU {
    pub fn new() -> Self {
        Self {
            apu_enable: false,
            dac_enable: 0,
            len_timers: [0; APU_CHANNELS],
            nr50: 0,
        }
    }
}

impl BusMember for APU {
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            // NR30: Channel 3 DAC enable
            0xFF1A => {
                if self.dac_enable & (1 << 2) != 0 {
                    0x80
                } else {
                    0x00
                }
            }

            // NR50: Master volume & VIN panning
            0xFF24 => self.nr50,

            // NR52: Sound on/off
            0xFF26 => {
                if !self.apu_enable {
                    0
                } else {
                    self.len_timers
                        .iter()
                        .enumerate()
                        .filter(|(_, &i)| i > 0)
                        .fold(0, |a, (ch, _)| a | (1 << ch))
                        & self.dac_enable
                        | 0x87_u8
                }
            }

            // Wave form RAM
            0xFF30..=0xFF3F => 0,

            _ => 0xFF,
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            // FF12 — NR12: Channel 1 volume & envelope
            0xFF12 => {
                if val & 0x07 == 0 {
                    self.dac_enable &= !(1 << 0);
                } else {
                    self.dac_enable |= 1 << 0;
                }
            }

            // FF17 — NR22: Channel 2 volume & envelope
            0xFF17 => {
                if val & 0x07 == 0 {
                    self.dac_enable &= !(1 << 1);
                } else {
                    self.dac_enable |= 1 << 1;
                }
            }

            // NR30: Channel 3 DAC enable
            0xFF1A => {
                if (val & 0x80) == 0x80 {
                    self.dac_enable |= 1 << 2;
                } else {
                    self.dac_enable &= !(1 << 2);
                }
            }

            // NR50: Master volume & VIN panning
            0xFF24 => self.nr50 = val,

            // Wave form RAM
            0xFF30..=0xFF3F => (),

            _ => (),
        }
    }
}

impl Tickable for APU {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // APU is not affected by double speed
        let ticks = ticks.get_t_no_ds();

        let t = if ticks > u8::MAX.into() {
            u8::MAX
        } else {
            ticks as u8
        };

        for l in self.len_timers.iter_mut() {
            if *l > t {
                *l -= t;
            } else {
                *l = 0;
            }
        }

        Ok(())
    }
}
