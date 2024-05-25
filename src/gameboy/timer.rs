use crate::cpu_sm83::cpu::CPU_CLOCK_HZ;
use crate::gameboy::bus::bus::BusMember;
use crate::gameboy::tickable::{Tickable, Ticks, ONE_MCYCLE};

use anyhow::Result;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

const TAC_ENABLE: u8 = 1 << 2;
const TAC_DIV_MASK: u8 = 0x03;
const TAC_MASK: u8 = 0x07;

#[derive(FromPrimitive)]
enum TimerInput {
    CPUDiv1024 = 0,
    CPUDiv16 = 1,
    CPUDiv64 = 2,
    CPUDiv256 = 3,
}

impl TimerInput {
    #[allow(dead_code)]
    pub fn get_hz(&self) -> usize {
        CPU_CLOCK_HZ / self.get_div()
    }

    pub fn get_div(&self) -> usize {
        match self {
            TimerInput::CPUDiv1024 => 1024,
            TimerInput::CPUDiv16 => 16,
            TimerInput::CPUDiv64 => 64,
            TimerInput::CPUDiv256 => 256,
        }
    }

    pub fn get_mask(&self) -> usize {
        self.get_div() >> 1
    }
}

pub struct Timer {
    cycles: usize,
    tima: u8,
    tma: u8,
    tac: u8,
    intreq: bool,

    /// Timer has overflowed this cycle (set for 1 M-cycle)
    overflow: bool,

    /// Timer has reloaded this cycle (set for 1 M-cycle)
    reloaded: bool,
}

impl Timer {
    pub fn from_div(div: u8) -> Self {
        Self {
            cycles: (div as usize) << 8,
            tima: 0,
            tma: 0,
            tac: 0,
            intreq: false,
            overflow: false,
            reloaded: false,
        }
    }

    pub fn new() -> Self {
        Self::from_div(0)
    }

    pub fn get_clr_intreq(&mut self) -> bool {
        let val = self.intreq;
        self.intreq = false;
        val
    }

    fn update_timer(&mut self, prev_bit: usize, new_bit: usize) {
        if self.tac & TAC_ENABLE != TAC_ENABLE {
            return;
        }

        if prev_bit != 0 && new_bit == 0 {
            self.tima = self.tima.wrapping_add(1);
            if self.tima == 0 {
                self.overflow = true;
            }
        }
    }

    fn update_cycles(&mut self, cycles: usize) {
        let mask = TimerInput::from_u8(self.tac & TAC_DIV_MASK)
            .unwrap()
            .get_mask();
        self.update_timer(self.cycles & mask, cycles & mask);

        self.cycles = cycles;
    }

    fn update_tac(&mut self, new_tac: u8) {
        // Timer glitch - switching input dividers may increment the timer
        let old_mask = TimerInput::from_u8(self.tac & TAC_DIV_MASK)
            .unwrap()
            .get_mask();
        self.update_timer(self.cycles & old_mask, 0);

        self.tac = new_tac;
    }
}

impl BusMember for Timer {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // DIV - Divider
            0xFF04 => ((self.cycles >> 8) & 0xFF) as u8,

            // TIMA - Timer counter
            0xFF05 => self.tima,

            // TMA - Timer counter reload register
            0xFF06 => self.tma,

            // TAC - Timer control
            0xFF07 => self.tac | !TAC_MASK,

            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // DIV - Divider
            0xFF04 => self.update_cycles(0),

            // TIMA - Timer counter
            0xFF05 => {
                // Timer quirk - writes on the same cycle as the timer is reloaded
                // are ignored.
                if !self.reloaded {
                    self.tima = val;
                    if self.overflow {
                        // Timer quirk - writes on the cycle the timer overflows
                        // stops the timer from reloading and triggering an interrupt.
                        self.overflow = false;
                    }
                }
            }

            // TMA - Timer counter reload register
            0xFF06 => {
                self.tma = val;

                // Timer quirk - if TMA is written on the cycle the timer is reloaded,
                // TIMA also gets written to the new value.
                if self.reloaded {
                    self.tima = self.tma;
                }
            }

            // TAC - Timer control
            0xFF07 => self.update_tac(val & TAC_MASK),

            _ => unreachable!(),
        }
    }
}

impl Tickable for Timer {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // Timer can run on double speed
        for _ in 0..ticks.get_t_ds() {
            if self.cycles % ONE_MCYCLE == 0 {
                // Timer reload quirk - actual reload happens
                // one M-cycle after overflow.
                if self.overflow {
                    self.tima = self.tma;
                    self.intreq = true;
                    self.overflow = false;
                    self.reloaded = true;
                } else if self.reloaded {
                    self.reloaded = false;
                }
            }

            self.update_cycles(self.cycles.wrapping_add(1));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn div() {
        let mut t = Timer::new();
        assert_eq!(t.read(0xFF04), 0);
        t.tick(Ticks::from_t(256)).unwrap();
        assert_eq!(t.read(0xFF04), 1);
        t.tick(Ticks::from_t(256 * 254)).unwrap();
        assert_eq!(t.read(0xFF04), 255);
        t.tick(Ticks::from_t(256)).unwrap();
        assert_eq!(t.read(0xFF04), 0);
    }

    #[test]
    fn div_reset() {
        let mut t = Timer::new();
        assert_eq!(t.read(0xFF04), 0);
        t.tick(Ticks::from_t(256)).unwrap();
        assert_eq!(t.read(0xFF04), 1);
        t.write(0xFF04, 123);
        assert_eq!(t.read(0xFF04), 0);
    }

    #[test]
    fn interrupt() {
        let mut t = Timer::new();
        t.write(0xFF07, 0x07);
        assert!(!t.get_clr_intreq());
        t.tick(Ticks::from_t(256 * 256)).unwrap();
        assert!(!t.get_clr_intreq());
        // Extra tick for timer reload quirk
        t.tick(Ticks::from_t(ONE_MCYCLE)).unwrap();
        assert!(t.get_clr_intreq());
        assert!(!t.get_clr_intreq());
    }

    #[test]
    fn counter() {
        let mut t = Timer::new();
        t.write(0xFF07, 0x07);
        assert_eq!(t.read(0xFF05), 0);
        t.tick(Ticks::from_t(256)).unwrap();
        assert_eq!(t.read(0xFF05), 1);
    }

    #[test]
    fn reload() {
        let mut t = Timer::new();
        t.write(0xFF07, 0x07);
        t.write(0xFF06, 0xAA);
        assert_eq!(t.read(0xFF05), 0);
        t.tick(Ticks::from_t(256 * 256)).unwrap();
        assert_eq!(t.read(0xFF05), 0);
        // Extra tick for timer reload quirk
        t.tick(Ticks::from_t(ONE_MCYCLE)).unwrap();
        assert_eq!(t.read(0xFF05), 0xAA);
    }
}
