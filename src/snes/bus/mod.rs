pub mod testbus;

use std::fmt;

use crate::tickable::Tickable;

/// Address data type (actually 24-bit)
pub type Address = u32;

/// Address mask
pub const ADDRESS_MASK: Address = 0x00FFFFFF;

/// Total address space
pub const ADDRESS_SPACE_SIZE: usize = 16 * 1024 * 1024;
pub const ADDRESS_SPACE: u32 = 16 * 1024 * 1024;

pub trait BusMember {
    fn read(&self, addr: Address) -> u8;
    fn write(&mut self, addr: Address, val: u8);

    /// Writes an entire u8 slice to the bus from a specified
    /// address offset.
    fn write_slice(&mut self, from: &[u8], offset: Address) {
        for (i, b) in from.into_iter().enumerate() {
            self.write(offset.wrapping_add(i as Address) & ADDRESS_MASK, *b);
        }
    }

    /// Reads the specified size from the specified address
    /// into a u8 vec.
    fn read_vec(&self, addr: Address, size: usize) -> Vec<u8> {
        let mut addr = addr;
        let mut ret: Vec<u8> = vec![];
        for _ in 0..size {
            ret.push(self.read(addr));
            addr = addr.wrapping_add(1) & ADDRESS_MASK;
        }
        ret
    }

    /// Write 16-bits to addr + 1 and addr (specific access order),
    /// in little endian.
    fn write16(&mut self, addr: Address, val: u16) {
        self.write(addr.wrapping_add(1), (val >> 8) as u8);
        self.write(addr, val as u8);
    }

    /// Write 16-bits to addr and addr + 1 (specific access order),
    /// in little endian.
    /// This access order is inverted, for operations that
    /// require that..
    fn write16_acc_low(&mut self, addr: Address, val: u16) {
        self.write(addr, val as u8);
        self.write(addr.wrapping_add(1), (val >> 8) as u8);
    }

    /// Read 16-bits from addr and addr + 1,
    /// from little endian.
    fn read16(&self, addr: Address) -> u16 {
        let l = self.read(addr);
        let h = self.read(addr.wrapping_add(1));
        l as u16 | (h as u16) << 8
    }
}

pub trait Bus: BusMember + fmt::Display + Tickable {
    /// Reads a memory location while ticking peripherals
    /// for the access time.
    fn read_tick(&mut self, addr: Address) -> u8 {
        let v = self.read(addr);
        self.tick(1).unwrap();
        v
    }

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 16-bits.
    fn read16_tick_a16(&mut self, addr: Address) -> u16 {
        let mut v = self.read(addr) as u16;
        self.tick(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        v |= (self.read(hi_addr) as u16) << 8;
        self.tick(1).unwrap();
        v
    }

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 24-bits.
    fn read16_tick_a24(&mut self, addr: Address) -> u16 {
        let mut v = self.read(addr) as u16;
        self.tick(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        v |= (self.read(hi_addr) as u16) << 8;
        self.tick(1).unwrap();
        v
    }

    /// Reads 24-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 16-bits.
    fn read24_tick_a16(&mut self, addr: Address) -> u32 {
        let mut v = self.read(addr) as u32;
        self.tick(1).unwrap();
        let mid_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        v |= (self.read(mid_addr) as u32) << 8;
        self.tick(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(2));
        v |= (self.read(hi_addr) as u32) << 16;
        self.tick(1).unwrap();
        v
    }

    /// Writes a memory location while ticking peripherals
    /// for the access time.
    fn write_tick(&mut self, addr: Address, val: u8) {
        self.write(addr, val);
        self.tick(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 16-bit address wrap.
    fn write16_tick_a16(&mut self, addr: Address, val: u16) {
        self.write(addr, (val & 0xFF) as u8);
        self.tick(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        self.write(hi_addr, (val >> 8) as u8);
        self.tick(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 16-bit address wrap, descending temporal order.
    fn write16_tick_a16_desc(&mut self, addr: Address, val: u16) {
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        self.write(hi_addr, (val >> 8) as u8);
        self.tick(1).unwrap();
        self.write(addr, (val & 0xFF) as u8);
        self.tick(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 24-bit address wrap.
    fn write16_tick_a24(&mut self, addr: Address, val: u16) {
        self.write(addr, (val & 0xFF) as u8);
        self.tick(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        self.write(hi_addr, (val >> 8) as u8);
        self.tick(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 24-bit address wrap, descending temporal order.
    fn write16_tick_a24_desc(&mut self, addr: Address, val: u16) {
        let hi_addr = addr.wrapping_add(1);
        self.write(hi_addr, (val >> 8) as u8);
        self.tick(1).unwrap();
        self.write(addr, (val & 0xFF) as u8);
        self.tick(1).unwrap();
    }
}

impl core::fmt::Debug for dyn Bus {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Bus")
    }
}

pub struct BusIterator<'a> {
    bus: &'a dyn Bus,
    next: Address,
}

impl<'a> BusIterator<'a> {
    pub fn new_from(bus: &'a dyn Bus, offset: Address) -> BusIterator {
        BusIterator { bus, next: offset }
    }

    pub fn new(bus: &'a dyn Bus) -> BusIterator {
        Self::new_from(bus, 0)
    }
}

impl<'a> Iterator for BusIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.next;
        self.next = self.next.wrapping_add(1) & ADDRESS_MASK;

        Some(self.bus.read(curr))
    }
}

#[cfg(test)]
mod tests {
    use super::testbus::Testbus;
    use super::*;

    fn testbus() -> Testbus {
        let mut b = Testbus::new();
        for a in 0..ADDRESS_SPACE {
            b.write(a, a as u8);
        }
        b
    }

    #[test]
    fn busiterator_new() {
        let b = testbus();
        let mut i = BusIterator::new(&b);

        for a in 0..=ADDRESS_MASK {
            assert_eq!(i.next(), Some(a as u8));
        }
        // Should wrap around at the end
        assert_eq!(i.next(), Some(0));
    }

    #[test]
    fn busiterator_new_from() {
        let b = testbus();
        let mut i = BusIterator::new_from(&b, 5);

        for a in 5..=ADDRESS_MASK {
            assert_eq!(i.next(), Some(a as u8));
        }
        // Should wrap around at the end
        assert_eq!(i.next(), Some(0));
    }

    #[test]
    fn bus_write16() {
        let mut b: Box<dyn Bus> = Box::new(testbus());
        b.write16(0x1000, 0x55AA);
        assert_eq!(b.read(0x1000), 0xAA);
        assert_eq!(b.read(0x1001), 0x55);
    }

    #[test]
    fn bus_read16() {
        let mut b: Box<dyn Bus> = Box::new(testbus());
        b.write(0x1000, 0xAA);
        b.write(0x1001, 0x55);
        assert_eq!(b.read16(0x1000), 0x55AA);
    }
}
