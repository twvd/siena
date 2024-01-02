use std::fmt;

use num_derive::ToPrimitive;
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, IntoEnumIterator};

/// Selector for A / B flag registers.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum Flags {
    A = 0,
    B = 1,
}

/// Bit positions of the flags in the A/B flag registers.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum Flag {
    /// Overflow 0
    OV0 = 0,
    /// Overflow 1
    OV1 = 1,
    /// Zero
    Z = 2,
    /// Carry
    C = 3,
    /// Sign 0
    S0 = 4,
    /// Sign 1
    S1 = 5,
}

/// Bit positions of the flags in the SR register.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum SR {
    /// Output 0
    O0 = 0,
    /// Output 1
    O1 = 1,
    // Bit 2 to 6 always 0
    /// Interrupt enable
    EI = 7,
    /// Serial Input Control
    /// 0 = 16-bit, 1 = 8-bit
    SIC = 8,
    /// Serial Output Control
    /// 0 = 16-bit, 1 = 8-bit
    SOC = 9,
    /// Data Register (DR) control
    /// 0 = 16-bit, 1 = 8-bit
    DRC = 10,
    /// DMA enable
    DMA = 11,
    /// DR Status
    DRS = 12,
    /// User Flag 0
    USF0 = 13,
    /// User Flag 1
    USF1 = 14,
    /// Request for Master
    RQM = 15,
}

/// Bit-width of a register (see Register::width())
#[derive(Debug, Eq, PartialEq)]
pub enum RegisterWidth {
    FourBit,
    SixteenBit,
}

/// Enumeration of registers
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Register {
    /// Accumulator A
    ACCA,
    /// Accumulator B
    ACCB,
    /// Sign register
    SGN,
    /// Multiplication operand 1
    K,
    /// Multiplication operand 2
    L,
    /// Multiplication result (upper 16-bit)
    M,
    /// Multiplication result (lower 16-bit)
    N,
    /// Temporary register
    TR,
    /// Temporary register B
    TRB,
    /// Data Register
    DR,
    /// Status Register
    SR,
    /// Program Counter
    PC,
    /// ROM Pointer
    RP,
    /// Data Pointer
    DP,
    /// Stack Pointer
    SP,
}

impl Register {
    /// Width of the register.
    pub const fn width(&self) -> RegisterWidth {
        match self {
            Register::SP => RegisterWidth::FourBit,
            _ => RegisterWidth::SixteenBit,
        }
    }
}

/// Complete CPU register file
#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct RegisterFile {
    pub acca: u16,
    pub accb: u16,
    pub sgn: u16,
    pub k: u16,
    pub l: u16,
    pub m: u16,
    pub n: u16,
    pub tr: u16,
    pub trb: u16,
    pub pc: u16,
    pub dp: u16,
    pub rp: u16,
    pub sp: u8,
    pub dr: u16,
    pub sr: u16,

    pub flags: [u8; 2],
}

impl RegisterFile {
    pub fn from_pc(pc: u16) -> Self {
        Self {
            acca: 0,
            accb: 0,
            sgn: 0,
            k: 0,
            l: 0,
            m: 0,
            n: 0,
            tr: 0,
            trb: 0,
            pc,
            dp: 0,
            rp: 0,
            sp: 0,
            dr: 0,
            sr: 0,
            flags: [0; 2],
        }
    }

    pub fn new() -> Self {
        Self::from_pc(0)
    }

    /// Write a value to a register.
    pub fn write(&mut self, reg: Register, val: u16) {
        let reg4 = || {
            assert!(val <= 0x0F);
            val as u8
        };
        let reg16 = || val;

        match reg {
            // Pure 16-bit registers
            Register::ACCA => self.acca = reg16(),
            Register::ACCB => self.accb = reg16(),
            Register::SGN => self.sgn = reg16(),
            Register::K => self.k = reg16(),
            Register::L => self.l = reg16(),
            Register::M => self.m = reg16(),
            Register::N => self.n = reg16(),
            Register::TR => self.tr = reg16(),
            Register::TRB => self.trb = reg16(),
            Register::PC => self.pc = reg16(),
            Register::DP => self.dp = reg16(),
            Register::RP => self.rp = reg16(),
            Register::DR => self.dr = reg16(),
            Register::SR => self.sr = reg16(),

            // 4-bit
            Register::SP => self.sp = reg4(),
        }
    }

    /// Read a 4-bit or 16-bit register.
    pub fn read(&self, reg: Register) -> u16 {
        match reg {
            // Pure 16-bit registers
            Register::ACCA => self.acca,
            Register::ACCB => self.accb,
            Register::SGN => self.sgn,
            Register::K => self.k,
            Register::L => self.l,
            Register::M => self.m,
            Register::N => self.n,
            Register::TR => self.tr,
            Register::TRB => self.trb,
            Register::DP => self.dp,
            Register::RP => self.rp,
            Register::DR => self.dr,
            Register::SR => self.sr,
            Register::PC => self.pc,

            // 4-bit
            Register::SP => self.sp.into(),
        }
    }

    /// Reads a 4-bit register
    pub fn read4(&self, reg: Register) -> u8 {
        assert_eq!(reg.width(), RegisterWidth::FourBit);
        self.read(reg) as u8
    }

    /// Reads a 16-bit register
    pub fn read16(&self, reg: Register) -> u16 {
        assert_eq!(reg.width(), RegisterWidth::SixteenBit);
        self.read(reg)
    }

    /// Test a flag in a flag register.
    pub fn test_flag(&self, flags_reg: Flags, f: Flag) -> bool {
        let flags_idx = flags_reg.to_usize().unwrap();
        self.flags[flags_idx] & (1u8 << f.to_u8().unwrap()) != 0
    }

    /// Clear and write the flags.
    pub fn write_flags(&mut self, flags_reg: Flags, flag_val: &[(Flag, bool)]) {
        let flags_idx = flags_reg.to_usize().unwrap();
        let mut p: u8 = self.flags[flags_idx];
        for &(b, on) in flag_val {
            let bit = 1u8 << b.to_u8().unwrap();
            p &= !bit;
            if on {
                p |= bit;
            }
        }
        self.flags[flags_idx] = p;
    }

    /// Clear and write SR.
    pub fn write_sr(&mut self, flag_val: &[(SR, bool)]) {
        let mut p: u16 = self.sr;
        for &(b, on) in flag_val {
            let bit = 1u16 << b.to_u16().unwrap();
            p &= !bit;
            if on {
                p |= bit;
            }
        }
        self.sr = p;
    }

    /// Test a flag in SR.
    pub fn test_sr(&self, f: SR) -> bool {
        self.sr & (1u16 << f.to_u16().unwrap()) != 0
    }
}

impl fmt::Display for RegisterFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let flags = self
            .flags
            .iter()
            .map(|v| {
                Flag::iter()
                    .map(|f| (f, f.to_string().chars().nth(0).unwrap()))
                    .map(|(f, fc)| {
                        if v & 1 << f.to_u8().unwrap() != 0 {
                            fc.to_uppercase().next().unwrap()
                        } else {
                            fc.to_lowercase().next().unwrap()
                        }
                    })
                    .collect::<String>()
            })
            .collect::<Vec<_>>();
        let sr = Flag::iter()
            .map(|f| (f, f.to_string().chars().nth(0).unwrap()))
            .map(|(f, fc)| {
                if self.sr & 1 << f.to_u16().unwrap() != 0 {
                    fc.to_uppercase().next().unwrap()
                } else {
                    fc.to_lowercase().next().unwrap()
                }
            })
            .collect::<String>();

        write!(
            f,
            "ACCA:{:04X} ACCB:{:04X} K:{:04X} L:{:04X} M:{:04X} N:{:04X} TR:{:04X} TRB:{:04X} SP:{:01X} PC:{:04X} DP:{:04X} RP:{:04X} SR:{:04X} ({}) Flg-A:{:02X} ({}) Flg-B:{:02X} ({})",
            self.acca,
            self.accb,
            self.k,
            self.l,
            self.m,
            self.n,
            self.tr,
            self.trb,
            self.sp,
            self.pc,
            self.dp,
            self.rp,
            self.sr,
            sr,
            self.flags[0],
            flags[0],
            self.flags[1],
            flags[1]
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_4bit() {
        let mut r = RegisterFile::new();
        r.write(Register::SP, 0x0F);
        assert_eq!(r.sp, 0x0F);
    }

    #[test]
    #[should_panic]
    fn write_4bit_error() {
        let mut r = RegisterFile::new();
        r.write(Register::SP, 0x1F);
    }

    #[test]
    fn write_16bit() {
        let mut r = RegisterFile::new();
        r.write(Register::ACCA, 0x1234);
        assert_eq!(r.acca, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::ACCB, 0x1234);
        assert_eq!(r.accb, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::K, 0x1234);
        assert_eq!(r.k, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::L, 0x1234);
        assert_eq!(r.l, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::M, 0x1234);
        assert_eq!(r.m, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::N, 0x1234);
        assert_eq!(r.n, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::TR, 0x1234);
        assert_eq!(r.tr, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::TRB, 0x1234);
        assert_eq!(r.trb, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::DR, 0x1234);
        assert_eq!(r.dr, 0x1234);

        let mut r = RegisterFile::new();
        r.write(Register::SR, 0x1234);
        assert_eq!(r.sr, 0x1234);
    }

    #[test]
    fn read4() {
        let mut r = RegisterFile::new();
        r.sp = 0x0A;
        assert_eq!(r.read4(Register::SP), 0x0A);
    }

    #[test]
    #[should_panic]
    fn read4_error() {
        let r = RegisterFile::new();
        r.read4(Register::ACCA);
    }

    #[test]
    fn read16() {
        let mut r = RegisterFile::new();
        r.acca = 0x1234;
        assert_eq!(r.read16(Register::ACCA), 0x1234);

        let mut r = RegisterFile::new();
        r.accb = 0x1234;
        assert_eq!(r.read16(Register::ACCB), 0x1234);

        let mut r = RegisterFile::new();
        r.k = 0x1234;
        assert_eq!(r.read16(Register::K), 0x1234);

        let mut r = RegisterFile::new();
        r.l = 0x1234;
        assert_eq!(r.read16(Register::L), 0x1234);

        let mut r = RegisterFile::new();
        r.m = 0x1234;
        assert_eq!(r.read16(Register::M), 0x1234);

        let mut r = RegisterFile::new();
        r.n = 0x1234;
        assert_eq!(r.read16(Register::N), 0x1234);

        let mut r = RegisterFile::new();
        r.tr = 0x1234;
        assert_eq!(r.read16(Register::TR), 0x1234);

        let mut r = RegisterFile::new();
        r.trb = 0x1234;
        assert_eq!(r.read16(Register::TRB), 0x1234);

        let mut r = RegisterFile::new();
        r.dr = 0x1234;
        assert_eq!(r.read16(Register::DR), 0x1234);

        let mut r = RegisterFile::new();
        r.sr = 0x1234;
        assert_eq!(r.read16(Register::SR), 0x1234);

        let mut r = RegisterFile::new();
        r.dp = 0x1234;
        assert_eq!(r.read16(Register::DP), 0x1234);

        let mut r = RegisterFile::new();
        r.rp = 0x1234;
        assert_eq!(r.read16(Register::RP), 0x1234);

        let mut r = RegisterFile::new();
        r.pc = 0x1234;
        assert_eq!(r.read16(Register::PC), 0x1234);
    }

    #[test]
    #[should_panic]
    fn read16_error() {
        let r = RegisterFile::new();
        r.read16(Register::SP);
    }

    #[test]
    fn write_flags_none() {
        let mut r = RegisterFile::new();
        r.flags[0] = 0xF0;
        r.write_flags(Flags::A, &[]);
        assert_eq!(r.flags[0], 0xF0);

        let mut r = RegisterFile::new();
        r.flags[1] = 0xF0;
        r.write_flags(Flags::B, &[]);
        assert_eq!(r.flags[1], 0xF0);
    }

    #[test]
    fn write_sr_none() {
        let mut r = RegisterFile::new();
        r.sr = 0xF0;
        r.write_sr(&[]);
        assert_eq!(r.sr, 0xF0);
    }

    #[test]
    fn write_flags_one_true() {
        let mut r = RegisterFile::new();
        r.write_flags(Flags::A, &[(Flag::Z, true)]);
        assert_eq!(r.flags[0], 1u8 << Flag::Z.to_u8().unwrap());
        assert_eq!(r.flags[1], 0);

        let mut r = RegisterFile::new();
        r.write_flags(Flags::B, &[(Flag::Z, true)]);
        assert_eq!(r.flags[1], 1u8 << Flag::Z.to_u8().unwrap());
        assert_eq!(r.flags[0], 0);
    }

    #[test]
    fn write_sr_one_true() {
        let mut r = RegisterFile::new();
        r.write_sr(&[(SR::EI, true)]);
        assert_eq!(r.sr, 1 << SR::EI.to_u16().unwrap());
        assert_eq!(r.flags[1], 0);
    }

    #[test]
    fn write_flags_one_false() {
        let mut r = RegisterFile::new();
        r.flags[0] = 1 << Flag::Z.to_u8().unwrap();
        r.write_flags(Flags::A, &[(Flag::Z, false)]);
        assert_eq!(r.flags[0], 0);
    }

    #[test]
    fn write_sr_one_false() {
        let mut r = RegisterFile::new();
        r.sr = 1 << SR::EI.to_u16().unwrap();
        r.write_sr(&[(SR::EI, false)]);
        assert_eq!(r.sr, 0);
    }

    #[test]
    fn write_flags_multiple_true() {
        let mut r = RegisterFile::new();
        r.write_flags(Flags::A, &[(Flag::Z, true), (Flag::C, true)]);
        assert_eq!(
            r.flags[0],
            1u8 << Flag::Z.to_u8().unwrap() | 1u8 << Flag::C.to_u8().unwrap()
        );
    }

    #[test]
    fn write_sr_multiple_true() {
        let mut r = RegisterFile::new();
        r.write_sr(&[(SR::EI, true), (SR::RQM, true)]);
        assert_eq!(
            r.sr,
            1 << SR::EI.to_u16().unwrap() | 1 << SR::RQM.to_u16().unwrap()
        );
    }

    #[test]
    fn write_flags_multiple_mixed() {
        let mut r = RegisterFile::new();
        r.write_flags(
            Flags::A,
            &[
                (Flag::Z, false),
                (Flag::C, false),
                (Flag::S0, true),
                (Flag::S1, true),
            ],
        );
        assert_eq!(
            r.flags[0],
            1u8 << Flag::S0.to_u8().unwrap() | 1u8 << Flag::S1.to_u8().unwrap()
        );
    }

    #[test]
    fn write_sr_multiple_mixed() {
        let mut r = RegisterFile::new();
        r.write_sr(&[
            (SR::O0, false),
            (SR::O1, false),
            (SR::EI, true),
            (SR::RQM, true),
        ]);
        assert_eq!(
            r.sr,
            1u16 << SR::EI.to_u16().unwrap() | 1u16 << SR::RQM.to_u16().unwrap()
        );
    }

    #[test]
    fn write_flags_unwritten_untouched() {
        let mut r = RegisterFile::new();
        r.write_flags(Flags::B, &[(Flag::Z, true)]);
        r.write_flags(Flags::B, &[(Flag::C, true)]);
        assert!(r.test_flag(Flags::B, Flag::Z));
    }

    #[test]
    fn write_sr_unwritten_untouched() {
        let mut r = RegisterFile::new();
        r.write_sr(&[(SR::O0, true)]);
        r.write_sr(&[(SR::O1, true)]);
        assert!(r.test_sr(SR::O0));
    }
}
