use std::fmt;

use anyhow::{bail, Result};
use num_derive::ToPrimitive;
use num_traits::ToPrimitive;

/// Datatype of a single CPU register.
type Reg = u8;

/// Bit positions of the flags in the F register.
#[derive(ToPrimitive, Debug, Copy, Clone)]
pub enum Flag {
    /// Zero
    Z = 7,
    /// Subtract
    N = 6,
    /// Half-carry
    H = 5,
    /// Carry
    C = 4,
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
    A,
    F,
    B,
    C,
    D,
    E,
    L,
    H,

    AF,
    BC,
    DE,
    HL,

    SP,
    PC,
}

impl Register {
    /// Width of the register.
    pub const fn width(&self) -> RegisterWidth {
        match self {
            Register::A
            | Register::F
            | Register::B
            | Register::C
            | Register::D
            | Register::E
            | Register::L
            | Register::H => RegisterWidth::EightBit,

            Register::AF
            | Register::BC
            | Register::DE
            | Register::HL
            | Register::SP
            | Register::PC => RegisterWidth::SixteenBit,
        }
    }
}

/// Complete CPU register file
#[derive(Debug, Eq, PartialEq)]
pub struct RegisterFile {
    /// A (accumulator) register.
    pub a: Reg,

    /// F (flags) register.
    pub f: Reg,

    pub b: Reg,
    pub c: Reg,
    pub d: Reg,
    pub e: Reg,

    /// H register, high byte of the 16-bit HL register.
    pub h: Reg,
    /// L register, low byte of the 16-bit HL register.
    pub l: Reg,

    /// Stack Pointer (SP)
    pub sp: u16,

    /// Program Counter (PC)
    pub pc: u16,
}

impl RegisterFile {
    pub fn new() -> Self {
        Self {
            a: 0,
            f: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0,
            pc: 0,
        }
    }

    /// Write a value to a register.
    /// Returns an error when attempting to write
    /// a 16-bit value to an 8-bit register.
    pub fn write(&mut self, reg: Register, val: u16) -> Result<()> {
        let reg8 = || {
            if val > u8::MAX.into() {
                bail!("Cannot write {} to 8-bit register {:?}", val, reg)
            } else {
                Ok(val as u8)
            }
        };
        let reg16 = || -> Result<(u8, u8)> {
            // MSB, LSB
            Ok(((val >> 8) as u8, (val & 0xFF) as u8))
        };
        let reg_af = || -> Result<(u8, u8)> {
            // MSB, LSB
            // Bottom 4 bits of F always read 0
            Ok(((val >> 8) as u8, (val & 0xF0) as u8))
        };

        match reg {
            // 8-bit single registers
            Register::A => self.a = reg8()?,
            Register::B => self.b = reg8()?,
            Register::C => self.c = reg8()?,
            Register::D => self.d = reg8()?,
            Register::E => self.e = reg8()?,
            // Bottom 4 bits of F always read 0
            Register::F => self.f = reg8()? & 0xF0,
            Register::H => self.h = reg8()?,
            Register::L => self.l = reg8()?,

            // 16-bit combination registers
            Register::AF => (self.a, self.f) = reg_af()?,
            Register::BC => (self.b, self.c) = reg16()?,
            Register::DE => (self.d, self.e) = reg16()?,
            Register::HL => (self.h, self.l) = reg16()?,

            // 16-bit-only registers
            Register::SP => self.sp = val,
            Register::PC => self.pc = val,
        }

        Ok(())
    }

    /// Write an 8-bit value to an 8-bit register.
    #[inline(always)]
    pub fn write8(&mut self, reg: Register, val: u8) -> Result<()> {
        self.write(reg, val.try_into()?)
    }

    /// Read an 8-bit or 16-bit register.
    pub fn read(&self, reg: Register) -> u16 {
        let reg16 = |msb: u8, lsb: u8| (msb as u16) << 8 | lsb as u16;

        match reg {
            // 8-bit single registers
            Register::A => self.a as u16,
            Register::B => self.b as u16,
            Register::C => self.c as u16,
            Register::D => self.d as u16,
            Register::E => self.e as u16,
            Register::F => self.f as u16,
            Register::H => self.h as u16,
            Register::L => self.l as u16,

            // 16-bit combination registers
            Register::AF => reg16(self.a, self.f),
            Register::BC => reg16(self.b, self.c),
            Register::DE => reg16(self.d, self.e),
            Register::HL => reg16(self.h, self.l),

            // 16-bit-only registers
            Register::SP => self.sp,
            Register::PC => self.pc,
        }
    }

    /// Reads an 8-bit register
    /// Returns an error if requested register is not 8-bit.
    pub fn read8(&self, reg: Register) -> Result<u8> {
        match reg.width() {
            RegisterWidth::EightBit => Ok(self.read(reg) as u8),
            _ => bail!("Attempting 8-bit read on 16-bit register {:?}", reg),
        }
    }

    /// Reads an 16-bit register
    /// Returns an error if requested register is not 16-bit.
    pub fn read16(&self, reg: Register) -> Result<u16> {
        match reg.width() {
            RegisterWidth::SixteenBit => Ok(self.read(reg)),
            _ => bail!("Attempting 16-bit read on 8-bit register {:?}", reg),
        }
    }

    /// Read register and increment.
    pub fn read_inc(&mut self, reg: Register) -> Result<u16> {
        match reg.width() {
            RegisterWidth::EightBit => Ok(self.read8_inc(reg)?.try_into()?),
            RegisterWidth::SixteenBit => Ok(self.read16_inc(reg)?),
        }
    }

    /// Read register and decrement.
    pub fn read_dec(&mut self, reg: Register) -> Result<u16> {
        match reg.width() {
            RegisterWidth::EightBit => Ok(self.read8_dec(reg)?.try_into()?),
            RegisterWidth::SixteenBit => Ok(self.read16_dec(reg)?),
        }
    }

    /// Read 8-bit register and increment.
    pub fn read8_inc(&mut self, reg: Register) -> Result<u8> {
        let val = self.read8(reg)?;
        self.write(reg, val.wrapping_add(1).try_into()?)?;
        Ok(val.try_into()?)
    }

    /// Read 16-bit register and increment.
    pub fn read16_inc(&mut self, reg: Register) -> Result<u16> {
        let val = self.read16(reg)?;
        self.write(reg, val.wrapping_add(1))?;
        Ok(val)
    }

    /// Read 8-bit register and decrement.
    pub fn read8_dec(&mut self, reg: Register) -> Result<u8> {
        let val = self.read8(reg)?;
        self.write(reg, val.wrapping_sub(1).try_into()?)?;
        Ok(val.try_into()?)
    }

    /// Read 16-bit register and decrement.
    pub fn read16_dec(&mut self, reg: Register) -> Result<u16> {
        let val = self.read16(reg)?;
        self.write(reg, val.wrapping_sub(1))?;
        Ok(val)
    }

    /// Clear and write the flags in F.
    pub fn write_flags(&mut self, flag_val: &[(Flag, bool)]) {
        let mut f: u8 = self.f;
        for &(b, on) in flag_val {
            // Just unwrap here, it will succeed anyway
            // because we know the enum values fit in u8.
            let bit = 1u8 << b.to_u8().unwrap();
            f &= !bit;
            if on {
                f |= bit;
            }
        }
        self.f = f & 0xF0;
    }

    /// Test a flag.
    pub fn test_flag(&self, f: Flag) -> bool {
        self.f & (1u8 << f.to_u8().unwrap()) != 0
    }
}

impl fmt::Display for RegisterFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} HL:{:04X} SP:{:04X} PC:{:04X}",
            self.a,
            self.f,
            self.b,
            self.c,
            self.d,
            self.e,
            self.read16(Register::HL).or(Err(fmt::Error))?,
            self.sp,
            self.pc
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_8bit() {
        let mut r = RegisterFile::new();
        r.write(Register::A, 0x12).unwrap();
        assert_eq!(r.a, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::B, 0x12).unwrap();
        assert_eq!(r.b, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::C, 0x12).unwrap();
        assert_eq!(r.c, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::D, 0x12).unwrap();
        assert_eq!(r.d, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::E, 0x12).unwrap();
        assert_eq!(r.e, 0x12);

        // Bottom 4-bits of F always read 0
        let mut r = RegisterFile::new();
        r.write(Register::F, 0xFF).unwrap();
        assert_eq!(r.f, 0xF0);

        let mut r = RegisterFile::new();
        r.write(Register::H, 0x12).unwrap();
        assert_eq!(r.h, 0x12);

        let mut r = RegisterFile::new();
        r.write(Register::L, 0x12).unwrap();
        assert_eq!(r.l, 0x12);
    }

    #[test]
    fn write_8bit_error() {
        let mut r = RegisterFile::new();
        assert!(matches!(r.write(Register::A, 0xFF), Ok(_)));
        assert_eq!(r.a, 0xFF);

        let mut r = RegisterFile::new();
        assert!(matches!(r.write(Register::A, 0x1FF), Err(_)));
        assert_eq!(r.a, 0);
    }

    #[test]
    fn write_comb16bit() {
        let mut r = RegisterFile::new();
        r.write(Register::AF, 0x1234).unwrap();
        // Bottom 4 bits of F always read 0
        assert_eq!((r.a, r.f), (0x12, 0x30));

        let mut r = RegisterFile::new();
        r.write(Register::BC, 0x1234).unwrap();
        assert_eq!((r.b, r.c), (0x12, 0x34));

        let mut r = RegisterFile::new();
        r.write(Register::DE, 0x1234).unwrap();
        assert_eq!((r.d, r.e), (0x12, 0x34));

        let mut r = RegisterFile::new();
        r.write(Register::HL, 0x1234).unwrap();
        assert_eq!((r.h, r.l), (0x12, 0x34));
    }

    #[test]
    fn write_16bit() {
        let mut r = RegisterFile::new();
        r.write(Register::SP, 0x1234).unwrap();
        assert_eq!(r.sp, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::PC, 0x1234).unwrap();
        assert_eq!(r.pc, 0x1234);
    }

    #[test]
    fn read8() {
        let mut r = RegisterFile::new();
        r.a = 0x12;
        assert!(matches!(r.read8(Register::A), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.b = 0x12;
        assert!(matches!(r.read8(Register::B), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.c = 0x12;
        assert!(matches!(r.read8(Register::C), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.d = 0x12;
        assert!(matches!(r.read8(Register::D), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.e = 0x12;
        assert!(matches!(r.read8(Register::E), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.f = 0x12;
        assert!(matches!(r.read8(Register::F), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.h = 0x12;
        assert!(matches!(r.read8(Register::H), Ok(0x12)));

        let mut r = RegisterFile::new();
        r.l = 0x12;
        assert!(matches!(r.read8(Register::L), Ok(0x12)));
    }

    #[test]
    fn read8_error() {
        let r = RegisterFile::new();
        assert!(matches!(r.read8(Register::AF), Err(_)));
    }

    #[test]
    fn read16() {
        let mut r = RegisterFile::new();
        (r.a, r.f) = (0x12, 0x34);
        assert!(matches!(r.read16(Register::AF), Ok(0x1234)));

        let mut r = RegisterFile::new();
        (r.b, r.c) = (0x12, 0x34);
        assert!(matches!(r.read16(Register::BC), Ok(0x1234)));

        let mut r = RegisterFile::new();
        (r.d, r.e) = (0x12, 0x34);
        assert!(matches!(r.read16(Register::DE), Ok(0x1234)));

        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x12, 0x34);
        assert!(matches!(r.read16(Register::HL), Ok(0x1234)));
    }

    #[test]
    fn read16_error() {
        let r = RegisterFile::new();
        assert!(matches!(r.read16(Register::A), Err(_)));
    }

    #[test]
    fn write_flags_low_nibble_reset() {
        let mut r = RegisterFile::new();
        r.f = 0xFF;
        r.write_flags(&[
            (Flag::N, false),
            (Flag::Z, false),
            (Flag::H, false),
            (Flag::C, false),
        ]);
        assert_eq!(r.f, 0x00);
    }

    #[test]
    fn write_flags_none() {
        let mut r = RegisterFile::new();
        r.f = 0xF0;
        r.write_flags(&[]);
        assert_eq!(r.f, 0xF0);
    }

    #[test]
    fn write_flags_one_true() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::Z, true)]);
        assert_eq!(r.f, 1u8 << Flag::Z.to_u8().unwrap());
    }

    #[test]
    fn write_flags_one_false() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::Z, false)]);
        assert_eq!(r.f, 0);
    }

    #[test]
    fn write_flags_multiple_true() {
        let mut r = RegisterFile::new();
        r.write_flags(&[(Flag::Z, true), (Flag::C, true)]);
        assert_eq!(
            r.f,
            1u8 << Flag::Z.to_u8().unwrap() | 1u8 << Flag::C.to_u8().unwrap()
        );
    }

    #[test]
    fn write_flags_multiple_mixed() {
        let mut r = RegisterFile::new();
        r.write_flags(&[
            (Flag::Z, false),
            (Flag::C, false),
            (Flag::H, true),
            (Flag::N, true),
        ]);
        assert_eq!(
            r.f,
            1u8 << Flag::H.to_u8().unwrap() | 1u8 << Flag::N.to_u8().unwrap()
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
        assert_eq!(r.read8_inc(Register::A).unwrap(), 12);
        assert_eq!(r.a, 13);
    }

    #[test]
    fn read8_inc_overflow() {
        let mut r = RegisterFile::new();
        r.a = 0xFF;
        assert_eq!(r.read8_inc(Register::A).unwrap(), 0xFF);
        assert_eq!(r.a, 0);
    }

    #[test]
    fn read8_dec() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read8_dec(Register::A).unwrap(), 12);
        assert_eq!(r.a, 11);
    }

    #[test]
    fn read8_dec_overflow() {
        let mut r = RegisterFile::new();
        r.a = 0;
        assert_eq!(r.read8_dec(Register::A).unwrap(), 0);
        assert_eq!(r.a, 0xFF);
    }

    #[test]
    fn read16_inc() {
        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x12, 0x34);
        assert_eq!(r.read16_inc(Register::HL).unwrap(), 0x1234);
        assert_eq!((r.h, r.l), (0x12, 0x35));
    }

    #[test]
    fn read16_inc_boundary() {
        // Check crossing 8-bit boundary
        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x00, 0xFF);
        assert_eq!(r.read16_inc(Register::HL).unwrap(), 0x00FF);
        assert_eq!((r.h, r.l), (0x01, 0x00));
    }

    #[test]
    fn read16_inc_overflow() {
        let mut r = RegisterFile::new();
        (r.h, r.l) = (0xFF, 0xFF);
        assert_eq!(r.read16_inc(Register::HL).unwrap(), 0xFFFF);
        assert_eq!((r.h, r.l), (0, 0));
    }

    #[test]
    fn read16_dec() {
        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x12, 0x34);
        assert_eq!(r.read16_dec(Register::HL).unwrap(), 0x1234);
        assert_eq!((r.h, r.l), (0x12, 0x33));
    }

    #[test]
    fn read16_dec_boundary() {
        // Check crossing 8-bit boundary
        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x01, 0x00);
        assert_eq!(r.read16_dec(Register::HL).unwrap(), 0x0100);
        assert_eq!((r.h, r.l), (0x00, 0xFF));
    }

    #[test]
    fn read16_dec_overflow() {
        let mut r = RegisterFile::new();
        (r.h, r.l) = (0, 0);
        assert_eq!(r.read16_dec(Register::HL).unwrap(), 0);
        assert_eq!((r.h, r.l), (0xFF, 0xFF));
    }

    #[test]
    fn read_inc() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read_inc(Register::A).unwrap(), 12);
        assert_eq!(r.a, 13);

        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x12, 0x34);
        assert_eq!(r.read_inc(Register::HL).unwrap(), 0x1234);
        assert_eq!((r.h, r.l), (0x12, 0x35));
    }

    #[test]
    fn read_dec() {
        let mut r = RegisterFile::new();
        r.a = 12;
        assert_eq!(r.read_dec(Register::A).unwrap(), 12);
        assert_eq!(r.a, 11);

        let mut r = RegisterFile::new();
        (r.h, r.l) = (0x12, 0x34);
        assert_eq!(r.read_dec(Register::HL).unwrap(), 0x1234);
        assert_eq!((r.h, r.l), (0x12, 0x33));
    }
}
