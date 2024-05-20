use anyhow::Result;

use super::bus::{Bus, BusMember};
use crate::gameboy::tickable::{Tickable, Ticks};

use std::cell::RefCell;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Access {
    Read,
    Write,
}

#[derive(Copy, Clone, Debug)]
pub struct TraceEntry {
    pub addr: u16,
    pub access: Access,
    pub val: u8,
    pub cycle: usize,
}

pub struct Testbus {
    mem: [u8; u16::MAX as usize + 1],
    trace: RefCell<Vec<TraceEntry>>,
    cycles: usize,
    trace_enabled: bool,
}

impl Testbus {
    pub fn new() -> Self {
        Testbus {
            mem: [0; u16::MAX as usize + 1],
            trace: RefCell::new(vec![]),
            cycles: 0,
            trace_enabled: false,
        }
    }

    pub fn from(data: &[u8]) -> Self {
        let mut ret = Testbus::new();
        ret.write_slice(data, 0);
        ret
    }

    pub fn reset_trace(&mut self) {
        self.trace.borrow_mut().clear();
        self.trace_enabled = true;
    }

    pub fn get_trace(&self) -> Vec<TraceEntry> {
        self.trace.borrow().clone()
    }
}

impl Bus for Testbus {}

impl BusMember for Testbus {
    fn read(&self, addr: u16) -> u8 {
        let val = self.mem[addr as usize];
        if self.trace_enabled {
            self.trace.borrow_mut().push(TraceEntry {
                addr,
                access: Access::Read,
                val,
                cycle: self.cycles,
            });
        }
        val
    }

    fn write(&mut self, addr: u16, val: u8) {
        if self.trace_enabled {
            self.trace.borrow_mut().push(TraceEntry {
                addr,
                access: Access::Write,
                val,
                cycle: self.cycles,
            });
        }
        self.mem[addr as usize] = val;
    }
}

impl Tickable for Testbus {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        self.cycles += ticks.get_t_ds();
        Ok(())
    }
}

impl fmt::Display for Testbus {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Result::Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn testbus() {
        let mut b = Testbus::new();

        for a in 0..=u16::MAX {
            assert_eq!(b.read(a), 0);
        }
        for a in 0..=u16::MAX {
            b.write(a, a as u8);
        }
        for a in 0..=u16::MAX {
            assert_eq!(b.read(a), a as u8);
        }
    }
}
