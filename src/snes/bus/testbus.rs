use anyhow::Result;

use super::{Address, Bus, BusMember, ADDRESS_MASK, ADDRESS_SPACE_SIZE};
use crate::tickable::{Tickable, Ticks};

use std::cell::RefCell;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Access {
    Read,
    Write,
}

#[derive(Copy, Clone, Debug)]
pub struct TraceEntry {
    pub addr: Address,
    pub access: Access,
    pub val: u8,
    pub cycle: usize,
}

pub struct Testbus {
    mem: Box<[u8]>,
    trace: RefCell<Vec<TraceEntry>>,
    pub cycles: usize,
    trace_enabled: bool,
}

impl Testbus {
    pub fn new() -> Self {
        Testbus {
            // Need allocation here, too large for stack.
            mem: vec![0; ADDRESS_SPACE_SIZE].into_boxed_slice(),
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
    fn read(&self, addr: Address) -> u8 {
        assert_eq!(addr & ADDRESS_MASK, addr);

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

    fn write(&mut self, addr: Address, val: u8) {
        assert_eq!(addr & ADDRESS_MASK, addr);

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
        self.cycles += ticks;
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
    use super::super::ADDRESS_SPACE;
    use super::*;

    #[test]
    fn testbus() {
        let mut b = Testbus::new();

        for a in 0..ADDRESS_SPACE {
            assert_eq!(b.read(a), 0);
        }
        for a in 0..ADDRESS_SPACE {
            b.write(a, a as u8);
        }
        for a in 0..ADDRESS_SPACE {
            assert_eq!(b.read(a), a as u8);
        }
    }
}
