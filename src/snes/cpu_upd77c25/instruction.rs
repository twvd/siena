use std::fmt;

use anyhow::Result;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use strum::{Display, EnumIter};
use thiserror::Error;

pub const INSTRUCTION_LEN: usize = 3;

#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display, Eq, PartialEq)]
pub enum InstructionType {
    Op = 0,
    Rt = 1,
    Jp = 2,
    Ld = 3,
}

#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display)]
pub enum PSelect {
    /// RAM
    RAM = 0,
    /// Internal Data Bus
    IDB = 0b01,
    /// M register
    M = 0b10,
    /// N register
    N = 0b11,
}

#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display, Eq, PartialEq)]
pub enum AluFunction {
    /// No ALU operation
    Nop = 0b0000,
    /// Bitwise OR
    Or = 0b0001,
    /// Bitwise AND
    And = 0b0010,
    /// Exclusive OR
    Xor = 0b0011,
    /// Subtract
    Sub = 0b0100,
    /// Add
    Add = 0b0101,
    /// Subtract with borrow
    Sbr = 0b0110,
    /// Add with carry
    Adc = 0b0111,
    /// Decrement ACC
    Dec = 0b1000,
    /// Increment ACC
    Inc = 0b1001,
    /// Complement ACC (one's complement)
    Cmp = 0b1010,
    /// 1-bit right shift
    Shr1 = 0b1011,
    /// 1-bit left shift
    Shl1 = 0b1100,
    /// 2-bit left shift
    Shl2 = 0b1101,
    /// 4-bit left shift
    Shl4 = 0b1110,
    /// 8-bit exchange
    Xchg = 0b1111,
}

/// OP/RT ASL (accumulator select?) field
#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display)]
pub enum ASL {
    ACCA = 0b0,
    ACCB = 0b1,
}

/// OP/RT DPL (Data Pointer Low?) field
#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display)]
pub enum DPL {
    /// No operation
    DPNOP = 0b00,
    /// Increment DPL
    DPINC = 0b01,
    /// Decrement DPL
    DPDEC = 0b10,
    /// Clear DPL
    DPCLR = 0b11,
}

/// OP/RT SRC (source) field
#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display)]
pub enum SRC {
    /// NON/TRB, the contents of TRB are also used if NON is specified
    TRB = 0b0000,
    /// Accumulator A
    ACCA = 0b0001,
    /// Accumulator B
    ACCB = 0b0010,
    /// TR register
    TR = 0b0011,
    /// DP register
    DP = 0b0100,
    /// ROM pointer
    RP = 0b0101,
    /// RO ROM output data
    RO = 0b0110,
    /// SGN sign register
    SGN = 0b0111,
    /// DR register
    DR = 0b1000,
    /// DR no flag
    /// FR to IDB, RQM not set. In DMA not set
    DRNF = 0b1001,
    /// Status Register? (seems incorrect in datasheet)
    SR = 0b1010,
    /// Serial In - Most Significant Bit first
    SIM = 0b1011,
    /// Serial In Least Significant Bit first (bit reversed)
    SIL = 0b1100,
    /// K register
    K = 0b1101,
    /// L register
    L = 0b1110,
    /// RAM
    MEM = 0b1111,
}

/// DST (destination) field
#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display, PartialEq, Eq)]
pub enum DST {
    /// No destination
    NON = 0b0000,
    /// Accumulator A
    ACCA = 0b0001,
    /// Accumulator B
    ACCB = 0b0010,
    /// TR register
    TR = 0b0011,
    /// DP register
    DP = 0b0100,
    /// ROM pointer
    RP = 0b0101,
    /// DR register
    DR = 0b0110,
    /// Status Register
    SR = 0b0111,
    /// Serial In - Most Significant Bit first
    SIM = 0b1000,
    /// Serial In Least Significant Bit first (bit reversed)
    SIL = 0b1001,
    /// K register
    K = 0b1010,
    /// IDB -> K, ROM -> L
    KLR = 0b1011,
    /// Hi RAM -> K, IDB -> L
    /// Contents of RAM specified by DP.6 = 1 to K, IDB in L (1, DP.5, DP.4, DP.3-DP.0)
    KLM = 0b1100,
    /// L register
    L = 0b1101,
    /// TRB register
    TRB = 0b1110,
    /// RAM
    MEM = 0b1111,
}

#[derive(EnumIter, FromPrimitive, Debug, Copy, Clone, Display, Eq, PartialEq)]
pub enum BrchCnd {
    JMP = 0b100000000,
    CALL = 0b101000000,
    JNCA = 0b010000000,
    JCA = 0b010000010,
    JNCB = 0b010000100,
    JCB = 0b010000110,
    JNZA = 0b010001000,
    JZA = 0b010001010,
    JNZB = 0b010001100,
    JZB = 0b010001110,
    JNOVA0 = 0b010010000,
    JOVA0 = 0b010010010,
    JNOVB0 = 0b010010100,
    JOVB0 = 0b010010110,
    JNOVA1 = 0b010011000,
    JOVA1 = 0b010011010,
    JNOVB1 = 0b010011100,
    JOVB1 = 0b010011110,
    JNSA0 = 0b010100000,
    JSA0 = 0b010100010,
    JNSB0 = 0b010100100,
    JSB0 = 0b010100110,
    JNSA1 = 0b010101000,
    JSA1 = 0b010101010,
    JNSB1 = 0b010101100,
    JSB1 = 0b010101110,
    JDPL0 = 0b010110000,
    JDPLN0 = 0b010110001,
    JDPLF = 0b010110010,
    JDPLNF = 0b010110011,
    JNSIAK = 0b010110100,
    JSIAK = 0b010110110,
    JNSOAK = 0b010111000,
    JSOAK = 0b010111010,
    JNRQM = 0b010111100,
    JRQM = 0b010111110,
}

#[derive(Debug, Error)]
enum DecodeErr {
    #[error("End of instruction stream")]
    EndOfStream,
}

#[derive(Debug, Copy, Clone)]
pub struct InstructionOpRt {
    opcode: u32,
}

impl InstructionOpRt {
    pub fn instr(&self) -> InstructionType {
        InstructionType::from_u32((self.opcode >> 22) & 0x03)
            .expect(format!("Invalid type in {:?}", self).as_str())
    }

    pub fn rt(&self) -> bool {
        self.instr() == InstructionType::Rt
    }

    pub fn pselect(&self) -> PSelect {
        PSelect::from_u32((self.opcode >> 20) >> 0x03)
            .expect(format!("Invalid PSelect in {:?}", self).as_str())
    }

    pub fn alu(&self) -> AluFunction {
        AluFunction::from_u32((self.opcode >> 16) & 0x07)
            .expect(format!("Invalid ALU in {:?}", self).as_str())
    }

    pub fn asl(&self) -> ASL {
        ASL::from_u32((self.opcode >> 15) & 0x01)
            .expect(format!("Invalid ASL in {:?}", self).as_str())
    }

    pub fn dpl(&self) -> DPL {
        DPL::from_u32((self.opcode >> 13) & 0x03)
            .expect(format!("Invalid DST in {:?}", self).as_str())
    }

    pub fn dphm(&self) -> u16 {
        ((self.opcode >> 9) & 0x0F) as u16
    }

    pub fn rpdcr(&self) -> bool {
        ((self.opcode >> 8) & 1) != 0
    }

    pub fn src(&self) -> SRC {
        SRC::from_u32((self.opcode >> 4) & 0x0F)
            .expect(format!("Invalid SRC in {:?}", self).as_str())
    }

    pub fn dst(&self) -> DST {
        DST::from_u32(self.opcode & 0x0F).expect(format!("Invalid DST in {:?}", self).as_str())
    }
}

impl fmt::Display for InstructionOpRt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:06X}] ", self.opcode)?;
        write!(
            f,
            "{:?} PSel:{} ALU:{} SRC:{} DST:{} RPDCR:{} ASL:{} DPL:{} DPHM:{:03X}",
            self.instr(),
            self.pselect(),
            self.alu(),
            self.src(),
            self.dst(),
            self.rpdcr(),
            self.asl(),
            self.dpl(),
            self.dphm()
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InstructionJp {
    opcode: u32,
}

impl InstructionJp {
    pub fn next_address(&self) -> u16 {
        (((self.opcode & 0x03) << 11) | ((self.opcode >> 2) & 0x3FF)) as u16
    }

    pub fn branch_cond(&self) -> BrchCnd {
        BrchCnd::from_u32((self.opcode >> 13) & 0x1FF)
            .expect(format!("Invalid BRCH CND in {:?}", self).as_str())
    }
}

impl fmt::Display for InstructionJp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:06X}] ", self.opcode)?;
        write!(f, "{:?} ${:04X}", self.branch_cond(), self.next_address())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InstructionLd {
    opcode: u32,
}

impl InstructionLd {
    pub fn dst(&self) -> DST {
        DST::from_u32(self.opcode & 0x0F).expect(format!("Invalid DST in {:?}", self).as_str())
    }

    pub fn imm16(&self) -> u16 {
        ((self.opcode >> 6) & 0xFFFF) as u16
    }
}

impl fmt::Display for InstructionLd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:06X}] ", self.opcode)?;
        write!(f, "LD {:?}, ${:04X}", self.dst(), self.imm16())
    }
}

/// A decoded instruction
#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Op(InstructionOpRt),
    Rt(InstructionOpRt),
    Jp(InstructionJp),
    Ld(InstructionLd),
}

impl Instruction {
    /// Try to decode a single instruction from an iterator.
    pub fn decode(stream: &mut impl Iterator<Item = u8>) -> Result<Instruction> {
        let mut opcode = stream.next().ok_or(DecodeErr::EndOfStream)? as u32;
        opcode |= (stream.next().ok_or(DecodeErr::EndOfStream)? as u32) << 8;
        opcode |= (stream.next().ok_or(DecodeErr::EndOfStream)? as u32) << 16;

        Ok(match InstructionType::from_u32(opcode >> 22).unwrap() {
            InstructionType::Op => Self::Op(InstructionOpRt { opcode }),
            InstructionType::Rt => Self::Rt(InstructionOpRt { opcode }),
            InstructionType::Jp => Self::Jp(InstructionJp { opcode }),
            InstructionType::Ld => Self::Ld(InstructionLd { opcode }),
        })
    }

    pub fn len(&self) -> usize {
        INSTRUCTION_LEN
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Ld(i) => write!(f, "{}", i),
            Instruction::Jp(i) => write!(f, "{}", i),
            Instruction::Op(i) => write!(f, "{}", i),
            Instruction::Rt(i) => write!(f, "{}", i),
        }
    }
}
