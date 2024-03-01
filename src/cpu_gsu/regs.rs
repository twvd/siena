use std::fmt;

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, IntoEnumIterator};

/// Bit positions of the flags in the SFR register.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum Flag {
    /// Interrupt Request
    IRQ = 15,
    /// WITH executed
    B = 12,
    /// Immediate High
    IH = 11,
    /// Immediate Low
    IL = 10,
    /// ALT2 set
    ALT2 = 9,
    /// ALT1 set
    ALT1 = 8,
    /// Reading ROM
    R = 6,
    /// Go (GSU is running)
    G = 5,
    /// Overflow
    V = 4,
    /// Sign
    S = 3,
    /// Carry
    C = 2,
    /// Zero
    Z = 1,
}

/// Bit positions of the flags in the CFGR register.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum CFGRFlag {
    /// Multiplier speed
    MS0 = 5,
    /// Interrupt on STOP mask
    IRQ = 7,
}

/// Bit positions of the flags in the POR register.
#[derive(EnumIter, ToPrimitive, Debug, Copy, Clone, Display)]
pub enum PORFlag {
    NotTransparent = 0,
    Dither = 1,
    ColorHighNibble = 2,
    ColorHighFreeze = 3,
    ObjMode = 4,
}

const SCMR_BPP_MASK: u8 = 0x03;
/// Color depth values in SCMR
#[derive(Clone, Copy, Debug, FromPrimitive, ToPrimitive, Eq, PartialEq)]
pub enum BPP {
    // BPP == number of bitplanes
    Two = 0,   // 4 colors
    Four = 1,  // 16 colors
    Eight = 3, // 256 colors
}
impl BPP {
    pub fn entries_per_palette(&self) -> u8 {
        1 << self.to_u8().unwrap()
    }
    pub fn num_bitplanes(&self) -> usize {
        match self {
            BPP::Two => 2,
            BPP::Four => 4,
            BPP::Eight => 8,
        }
    }
}

const SCMR_HEIGHT_MASK: u8 = 0b00100100;
/// Screen height values in SCMR
#[derive(Clone, Copy, Debug, ToPrimitive, FromPrimitive, Eq, PartialEq)]
pub enum ScreenHeight {
    H128 = 0,
    H160 = 0b100,
    H192 = 0b100000,
    Obj = 0b100100,
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
    // General purpose registers
    /// Source/dest register
    R0,
    /// Pixel plot X register
    R1,
    /// Pixel plot Y register
    R2,
    /// General use
    R3,
    /// Lower 16-bit result of LMULT
    R4,
    /// General use
    R5,
    /// Multiplier for LMULT/FMULT
    R6,
    /// Fixed point texel X position for merge
    R7,
    /// Fixed point texel Y position for merge
    R8,
    /// General use
    R9,
    /// General use
    R10,
    /// Return address set by link
    R11,
    /// Loop counter
    R12,
    /// Loop point address
    R13,
    /// ROM address for GETBx
    R14,
    /// Program counter
    R15,

    // Control registers
    /// Status Flag Register
    SFR,
    /// Backup RAM register
    BRAMBR,
    /// Program Bank Register
    PBR,
    /// ROM bank register
    ROMBR,
    /// Configuration flag register
    CFGR,
    /// Screen base register
    SCBR,
    /// Clock speed register
    CLSR,
    /// Screen mode register
    SCMR,
    /// Version code register
    VCR,
    /// RAM bank register
    RAMBR,
    /// Cache base register
    CBR,

    // Plotting registers
    /// Color register
    COLR,
    /// Plotting option register
    POR,
}

impl Register {
    /// Width of the register.
    pub const fn width(&self) -> RegisterWidth {
        match self {
            Register::R0
            | Register::R1
            | Register::R2
            | Register::R3
            | Register::R4
            | Register::R5
            | Register::R6
            | Register::R7
            | Register::R8
            | Register::R9
            | Register::R10
            | Register::R11
            | Register::R12
            | Register::R13
            | Register::R14
            | Register::R15
            | Register::SFR
            | Register::CBR => RegisterWidth::SixteenBit,
            Register::BRAMBR
            | Register::PBR
            | Register::ROMBR
            | Register::CFGR
            | Register::SCBR
            | Register::CLSR
            | Register::SCMR
            | Register::VCR
            | Register::RAMBR
            | Register::COLR
            | Register::POR => RegisterWidth::EightBit,
        }
    }
}

/// Complete CPU register file
#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct RegisterFile {
    pub r: [u16; 16],
    pub sfr: u16,
    pub brambr: u8,
    pub pbr: u8,
    pub rombr: u8,
    pub cfgr: u8,
    pub scbr: u8,
    pub clsr: u8,
    pub scmr: u8,
    pub rambr: u8,
    pub cbr: u16,
    pub colr: u8,
    pub por: u8,

    /// Shadow R15 written by the executed code to emulate delay slots
    /// in all scenarios.
    r15_shadow: Option<u16>,
}

impl RegisterFile {
    pub fn new() -> Self {
        Self {
            r: [0; 16],
            sfr: 0,
            brambr: 0,
            pbr: 0,
            rombr: 0,
            cfgr: 0,
            scbr: 0,
            clsr: 0,
            scmr: 0,
            rambr: 0,
            cbr: 0,
            colr: 0,
            por: 0,
            r15_shadow: None,
        }
    }

    /// Write a value to a register.
    pub fn write(&mut self, reg: Register, val: u16) {
        let reg8 = || {
            assert!(val <= u8::MAX.into());
            val as u8
        };
        let reg16 = || val;

        match reg {
            // Pure 8-bit registers
            Register::PBR => self.pbr = reg8(),
            Register::ROMBR => self.rombr = reg8(),
            Register::CFGR => self.cfgr = reg8(),
            Register::SCBR => self.scbr = reg8(),
            Register::CLSR => self.clsr = reg8(),
            Register::SCMR => self.scmr = reg8(),
            Register::VCR => (),
            Register::RAMBR => self.rambr = reg8(),
            Register::BRAMBR => self.brambr = reg8(),
            Register::COLR => self.colr = reg8(),
            Register::POR => self.por = reg8(),

            // 16-bit registers
            Register::R0 => self.r[0] = reg16(),
            Register::R1 => self.r[1] = reg16(),
            Register::R2 => self.r[2] = reg16(),
            Register::R3 => self.r[3] = reg16(),
            Register::R4 => self.r[4] = reg16(),
            Register::R5 => self.r[5] = reg16(),
            Register::R6 => self.r[6] = reg16(),
            Register::R7 => self.r[7] = reg16(),
            Register::R8 => self.r[8] = reg16(),
            Register::R9 => self.r[9] = reg16(),
            Register::R10 => self.r[10] = reg16(),
            Register::R11 => self.r[11] = reg16(),
            Register::R12 => self.r[12] = reg16(),
            Register::R13 => self.r[13] = reg16(),
            Register::R14 => self.r[14] = reg16(),
            Register::R15 => self.r15_shadow = Some(reg16()),
            Register::SFR => self.sfr = reg16(),
            Register::CBR => self.cbr = reg16() & 0xFFF0,
        }
    }

    /// Read an 8-bit or 16-bit register.
    pub fn read(&self, reg: Register) -> u16 {
        match reg {
            // Pure 8-bit registers
            Register::PBR => self.pbr.into(),
            Register::ROMBR => self.rombr.into(),
            Register::CFGR => self.cfgr.into(),
            Register::SCBR => self.scbr.into(),
            Register::CLSR => self.clsr.into(),
            Register::SCMR => self.scmr.into(),
            Register::VCR => 1,
            Register::RAMBR => 0, //self.rambr.into(),
            Register::BRAMBR => self.brambr.into(),
            Register::COLR => self.colr.into(),
            Register::POR => self.por.into(),

            // 16-bit registers
            Register::R0 => self.r[0],
            Register::R1 => self.r[1],
            Register::R2 => self.r[2],
            Register::R3 => self.r[3],
            Register::R4 => self.r[4],
            Register::R5 => self.r[5],
            Register::R6 => self.r[6],
            Register::R7 => self.r[7],
            Register::R8 => self.r[8],
            Register::R9 => self.r[9],
            Register::R10 => self.r[10],
            Register::R11 => self.r[11],
            Register::R12 => self.r[12],
            Register::R13 => self.r[13],
            Register::R14 => self.r[14],
            Register::R15 => self.r[15],
            Register::SFR => self.sfr,
            Register::CBR => self.cbr,
        }
    }

    /// Reads an 8-bit register
    pub fn read8(&self, reg: Register) -> u8 {
        assert_eq!(reg.width(), RegisterWidth::EightBit);
        self.read(reg) as u8
    }

    /// Writes an 8-bit register
    pub fn write8(&mut self, reg: Register, val: u8) {
        assert_eq!(reg.width(), RegisterWidth::EightBit);
        self.write(reg, val as u16)
    }

    /// Reads an 16-bit register
    pub fn read16(&self, reg: Register) -> u16 {
        assert_eq!(reg.width(), RegisterWidth::SixteenBit);
        self.read(reg)
    }

    /// Read an Rxx register
    pub fn read_r(&self, r: usize) -> u16 {
        self.r[r]
    }

    /// Write an Rxx register
    pub fn write_r(&mut self, r: usize, val: u16) {
        if r == 15 {
            self.r15_shadow = Some(val);
        } else {
            self.r[r] = val;
        }
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

    /// Clear and write the flags in SFR.
    pub fn write_flags(&mut self, flag_val: &[(Flag, bool)]) {
        let mut p: u16 = self.sfr;
        for &(b, on) in flag_val {
            let bit = 1u16 << b.to_u16().unwrap();
            p &= !bit;
            if on {
                p |= bit;
            }
        }
        self.sfr = p;
    }

    /// Test a flag in SFR.
    pub fn test_flag(&self, f: Flag) -> bool {
        self.sfr & (1u16 << f.to_u16().unwrap()) != 0
    }

    /// Test a flag in CFGR.
    pub fn test_cfgr(&self, f: CFGRFlag) -> bool {
        self.cfgr & (1u8 << f.to_u8().unwrap()) != 0
    }

    /// Test a flag in POR.
    pub fn test_por(&self, f: PORFlag) -> bool {
        self.por & (1u8 << f.to_u8().unwrap()) != 0
    }

    pub fn get_scmr_bpp(&self) -> BPP {
        BPP::from_u8(self.scmr & SCMR_BPP_MASK).unwrap()
    }

    pub fn get_scmr_height(&self) -> ScreenHeight {
        ScreenHeight::from_u8(self.scmr & SCMR_HEIGHT_MASK).unwrap()
    }

    pub fn get_clr_r15_shadow(&mut self) -> Option<u16> {
        let v = self.r15_shadow;
        self.r15_shadow = None;
        v
    }

    pub fn get_r15(&self) -> u16 {
        self.r[15]
    }

    pub fn set_r15(&mut self, v: u16) {
        self.r[15] = v;
    }
}

impl fmt::Display for RegisterFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let flags: String = Flag::iter()
            .map(|f| (f, f.to_string().chars().nth(0).unwrap()))
            .map(|(f, fc)| {
                if self.sfr & 1 << f.to_u16().unwrap() != 0 {
                    fc.to_uppercase().next().unwrap()
                } else {
                    fc.to_lowercase().next().unwrap()
                }
            })
            .collect::<String>();

        let regs = (0..=15)
            .map(|r| format!("R{:02}:{:04X} ", r, self.read_r(r)))
            .collect::<String>();

        write!(f, "{} SFR: {:04X} ({})", regs, self.sfr, flags)
    }
}
