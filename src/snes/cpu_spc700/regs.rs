use std::fmt;

use num_derive::ToPrimitive;
use num_traits::ToPrimitive;
use strum::{Display, EnumIter, IntoEnumIterator};

/// Bit positions of the flags in the P register.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum Flag {
    /// Negative
    N = 7,
    /// Overflow
    V = 6,
    /// Direct Page
    P = 5,
    /// Break
    B = 4,
    /// Half carry
    H = 3,
    /// Interrupt enable
    I = 2,
    /// Zero
    Z = 1,
    /// Carry
    C = 0,
}

/// Bit-width of a register (see Register::width())
#[derive(Debug, Eq, PartialEq)]
pub enum RegisterWidth {
    EightBit,
    SixteenBit,
}

/// Enumeration of registers
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Register {
    /// Accumulator (lower 8-bit)
    A,
    /// Program Counter (full 16-bit)
    PC,
    /// Program Counter (lower 8-bit)
    PCL,
    /// Program Counter (upper 8-bit)
    PCH,
    /// Processor Status register
    PSW,
    /// Stack pointer
    SP,
    /// X-index
    X,
    /// Y-index
    Y,
}

impl Register {
    /// Width of the register.
    pub const fn width(&self) -> RegisterWidth {
        match self {
            Register::PC => RegisterWidth::SixteenBit,
            _ => RegisterWidth::EightBit,
        }
    }
}

/// Complete CPU register file
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RegisterFile {
    pub a: u8,
    pub pc: u16,
    pub psw: u8,
    pub sp: u8,
    pub x: u8,
    pub y: u8,
}

impl RegisterFile {
    pub fn new() -> Self {
        Self {
            a: 0,
            pc: 0,
            psw: 0,
            sp: 0,
            x: 0,
            y: 0,
        }
    }

    /// Write a value to a register.
    pub fn write(&mut self, reg: Register, val: u16) {
        let reg8 = || {
            assert!(val <= u8::MAX.into());
            val as u8
        };
        let reg16 = || val;
        let reg16_high = |old: u16| {
            // Replace MSB
            assert!(val <= u8::MAX.into());
            old & 0xFF | (val << 8)
        };
        let reg16_low = |old: u16| {
            // Replace LSB
            assert!(val <= u8::MAX.into());
            val & 0xFF | (old & 0xFF00)
        };

        match reg {
            // Pure 8-bit registers
            Register::A => self.a = reg8(),
            Register::PSW => self.psw = reg8(),
            Register::X => self.x = reg8(),
            Register::Y => self.y = reg8(),
            Register::SP => self.sp = reg8(),

            // Lower/higher 8-bit of 16-bit registers
            Register::PCL => self.pc = reg16_low(self.pc),
            Register::PCH => self.pc = reg16_high(self.pc),

            // 16-bit registers
            Register::PC => self.pc = reg16(),
        }
    }

    /// Read an 8-bit or 16-bit register.
    pub fn read(&self, reg: Register) -> u16 {
        let reg16_low = |r| r & 0xFF;
        let reg16_high = |r| r >> 8;

        match reg {
            // Pure 8-bit registers
            Register::A => self.a.into(),
            Register::PSW => self.psw.into(),
            Register::X => self.x.into(),
            Register::Y => self.y.into(),
            Register::SP => self.sp.into(),

            // Lower/higher 8-bit of 16-bit registers
            Register::PCL => reg16_low(self.pc),
            Register::PCH => reg16_high(self.pc),

            // 16-bit registers
            Register::PC => self.pc,
        }
    }

    /// Reads an 8-bit register
    pub fn read8(&self, reg: Register) -> u8 {
        assert_eq!(reg.width(), RegisterWidth::EightBit);
        self.read(reg) as u8
    }

    /// Reads an 16-bit register
    pub fn read16(&self, reg: Register) -> u16 {
        assert_eq!(reg.width(), RegisterWidth::SixteenBit);
        self.read(reg)
    }

    /// Read register and post-increment.
    pub fn read_inc(&mut self, reg: Register) -> u16 {
        match reg.width() {
            RegisterWidth::EightBit => self.read8_inc(reg) as u16,
            RegisterWidth::SixteenBit => self.read16_inc(reg),
        }
    }

    /// Read register and post-decrement.
    pub fn read_dec(&mut self, reg: Register) -> u16 {
        match reg.width() {
            RegisterWidth::EightBit => self.read8_dec(reg).into(),
            RegisterWidth::SixteenBit => self.read16_dec(reg),
        }
    }

    /// Read 8-bit register and post-increment.
    pub fn read8_inc(&mut self, reg: Register) -> u8 {
        let val = self.read8(reg);
        self.write(reg, val.wrapping_add(1) as u16);
        val
    }

    /// Read 16-bit register and post-increment.
    pub fn read16_inc(&mut self, reg: Register) -> u16 {
        let val = self.read16(reg);
        self.write(reg, val.wrapping_add(1));
        val
    }

    /// Read 8-bit register and post-decrement.
    pub fn read8_dec(&mut self, reg: Register) -> u8 {
        let val = self.read8(reg);
        self.write(reg, val.wrapping_sub(1) as u16);
        val
    }

    /// Read 16-bit register and post-decrement.
    pub fn read16_dec(&mut self, reg: Register) -> u16 {
        let val = self.read16(reg);
        self.write(reg, val.wrapping_sub(1));
        val
    }

    /// Clear and write the flags in PSW.
    pub fn write_flags(&mut self, flag_val: &[(Flag, bool)]) {
        let mut p: u8 = self.psw;
        for &(b, on) in flag_val {
            let bit = 1u8 << b.to_u8().unwrap();
            p &= !bit;
            if on {
                p |= bit;
            }
        }
        self.psw = p;
    }

    /// Test a flag in P.
    pub fn test_flag(&self, f: Flag) -> bool {
        self.psw & (1u8 << f.to_u8().unwrap()) != 0
    }
}

impl fmt::Display for RegisterFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let flags: String = Flag::iter()
            .map(|f| (f, f.to_string().chars().nth(0).unwrap()))
            .map(|(f, fc)| {
                if self.psw & 1 << f.to_u8().unwrap() != 0 {
                    fc.to_uppercase().next().unwrap()
                } else {
                    fc.to_lowercase().next().unwrap()
                }
            })
            .collect::<String>();

        write!(
            f,
            "A:{:02X} PC:{:04X} SP:{:02X} X:{:02X} Y:{:02X} PSW:{:02X} ({})",
            self.a, self.pc, self.sp, self.x, self.y, self.psw, flags
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_8bit() {
        let mut r = RegisterFile::new();
        r.write(Register::PSW, 0x12);
        assert_eq!(r.psw, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::A, 0x12);
        assert_eq!(r.a, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::X, 0x12);
        assert_eq!(r.x, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::Y, 0x12);
        assert_eq!(r.y, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::SP, 0x12);
        assert_eq!(r.sp, 0x12);
    }

    #[test]
    #[should_panic]
    fn write_8bit_error() {
        let mut r = RegisterFile::new();
        r.write(Register::A, 0x1FF);
    }

    #[test]
    fn write_16bit() {
        let mut r = RegisterFile::new();
        r.write(Register::PC, 0x1234);
        assert_eq!(r.pc, 0x1234);
    }

    #[test]
    fn write_16bit_h() {
        let mut r = RegisterFile::new();
        r.pc = 0xEEFF;
        r.write(Register::PCH, 0x12);
        assert_eq!(r.pc, 0x12FF);
    }

    #[test]
    fn write_16bit_l() {
        let mut r = RegisterFile::new();
        r.pc = 0xEEFF;
        r.write(Register::PCL, 0x12);
        assert_eq!(r.pc, 0xEE12);
    }

    #[test]
    fn read8() {
        let mut r = RegisterFile::new();
        r.a = 0x12;
        assert_eq!(r.read8(Register::A), 0x12);

        let mut r = RegisterFile::new();
        r.x = 0x12;
        assert_eq!(r.read8(Register::X), 0x12);

        let mut r = RegisterFile::new();
        r.y = 0x12;
        assert_eq!(r.read8(Register::Y), 0x12);

        let mut r = RegisterFile::new();
        r.sp = 0x12;
        assert_eq!(r.read8(Register::SP), 0x12);
    }

    #[test]
    #[should_panic]
    fn read8_error() {
        let r = RegisterFile::new();
        r.read8(Register::PC);
    }

    #[test]
    fn read16() {
        let mut r = RegisterFile::new();
        r.pc = 0x1234;
        assert_eq!(r.read16(Register::PC), 0x1234);
    }

    #[test]
    #[should_panic]
    fn read16_error() {
        let r = RegisterFile::new();
        r.read16(Register::A);
    }

    #[test]
    fn write_flags_none() {
        let mut r = RegisterFile::new();
        r.psw = 0xF0;
        r.write_flags(&[]);
        assert_eq!(r.psw, 0xF0);
    }

    #[test]
    fn write_flags_one_true() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::N, true)]);
        assert_eq!(r.psw, 1u8 << Flag::N.to_u8().unwrap());
    }

    #[test]
    fn write_flags_one_false() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::N, false)]);
        assert_eq!(r.psw, 0);
    }

    #[test]
    fn write_flags_multiple_true() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::N, true), (Flag::C, true)]);
        assert_eq!(
            r.psw,
            1u8 << Flag::N.to_u8().unwrap() | 1u8 << Flag::C.to_u8().unwrap()
        );
    }

    #[test]
    fn write_flags_multiple_mixed() {
        let mut r = RegisterFile::new();
        r.write_flags(&[
            (Flag::Z, false),
            (Flag::C, false),
            (Flag::V, true),
            (Flag::N, true),
        ]);
        assert_eq!(
            r.psw,
            1u8 << Flag::V.to_u8().unwrap() | 1u8 << Flag::N.to_u8().unwrap()
        );
    }

    #[test]
    fn write_flags_unwritten_untouched() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::Z, true)]);
        r.write_flags(&[(Flag::N, true)]);
        assert!(r.test_flag(Flag::Z));
    }

    #[test]
    fn read8_inc() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read8_inc(Register::A), 12);
        assert_eq!(r.a, 13);
    }

    #[test]
    fn read8_inc_overflow() {
        let mut r = RegisterFile::new();
        r.a = 0xFF;
        assert_eq!(r.read8_inc(Register::A), 0xFF);
        assert_eq!(r.a, 0);
    }

    #[test]
    fn read8_dec() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read8_dec(Register::A), 12);
        assert_eq!(r.a, 11);
    }

    #[test]
    fn read8_dec_overflow() {
        let mut r = RegisterFile::new();
        r.a = 0;
        assert_eq!(r.read8_dec(Register::A), 0);
        assert_eq!(r.a, 0xFF);
    }

    #[test]
    fn read16_inc() {
        let mut r = RegisterFile::new();
        r.pc = 0x1234;
        assert_eq!(r.read16_inc(Register::PC), 0x1234);
        assert_eq!(r.pc, 0x1235);
    }

    #[test]
    fn read16_inc_overflow() {
        let mut r = RegisterFile::new();
        r.pc = 0xFFFF;
        assert_eq!(r.read16_inc(Register::PC), 0xFFFF);
        assert_eq!(r.pc, 0);
    }

    #[test]
    fn read16_dec() {
        let mut r = RegisterFile::new();
        r.pc = 0x1234;
        assert_eq!(r.read16_dec(Register::PC), 0x1234);
        assert_eq!(r.pc, 0x1233);
    }

    #[test]
    fn read16_dec_overflow() {
        let mut r = RegisterFile::new();
        r.pc = 0;
        assert_eq!(r.read16_dec(Register::PC), 0);
        assert_eq!(r.pc, 0xFFFF);
    }

    #[test]
    fn read_inc() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read_inc(Register::A), 12);
        assert_eq!(r.a, 13);

        let mut r = RegisterFile::new();
        r.pc = 0x1234;
        assert_eq!(r.read_inc(Register::PC), 0x1234);
        assert_eq!(r.pc, 0x1235);
    }

    #[test]
    fn read_dec() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read_dec(Register::A), 12);
        assert_eq!(r.a, 11);

        let mut r = RegisterFile::new();
        r.pc = 0x1234;
        assert_eq!(r.read_dec(Register::PC), 0x1234);
        assert_eq!(r.pc, 0x1233);
    }
}
