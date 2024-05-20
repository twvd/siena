use std::fmt;

use anyhow::{bail, Context, Result};
use thiserror::Error;

use super::instructions::{InstructionType, INSTRUCTIONS, INSTRUCTIONS_CB};
use super::regs::Register;

/// A single operand to an instruction
#[derive(Debug)]
pub enum Operand {
    None,
    Constant(u8),
    Register(Register),
    RegisterIndirect(Register),
    RegisterIndirectInc(Register),
    RegisterIndirectDec(Register),
    Immediate8,
    Immediate16,
    ImmediateIndirect8,
    ImmediateIndirect16,
    Relative8,
    SPRelative8,
}

/// A value of an immediate operand of an instruction
#[derive(Copy, Clone)]
pub enum ImmediateVal {
    None,
    Immediate8(u8),
    Immediate16(u16),
}

impl fmt::Display for ImmediateVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => write!(f, "(none)"),
            Self::Immediate8(i) => write!(f, "${:02X}", i),
            Self::Immediate16(i) => write!(f, "${:04X}", i),
        }
    }
}

/// A definition for a single instruction of the CPU instruction set
pub struct InstructionDef {
    /// Mnemonic as string
    pub mnemonic: &'static str,

    /// Operand or Operand::None if not applicable for instruction.
    pub operands: [Operand; 2],

    /// Length of the complete instruction, including immediate values, in bytes.
    pub len: usize,

    /// Amount of cycles spent on executing the instruction.
    ///
    /// For conditional instructions, cycles[0] contains the
    /// cycles spent if the condition is met and cycles[1]
    /// if not.
    ///
    /// For unconditional instructions, use cycles[0].
    pub cycles: [usize; 2],

    /// Instruction group type.
    pub instr_type: InstructionType,
}

#[derive(Debug, Error)]
enum DecodeErr {
    #[error("End of instruction stream")]
    EndOfStream,
}

/// A decoded instruction.
pub struct Instruction {
    /// Reference to the definition.
    pub def: &'static InstructionDef,

    /// Immediate values, if applicable.
    pub immediate: [ImmediateVal; 2],

    /// Length of the full instruction.
    pub len: usize,

    /// Raw instruction bytes.
    pub raw: Vec<u8>,
}

impl Instruction {
    /// Try to decode a single instruction from an
    /// iterator.
    pub fn decode(stream: &mut impl Iterator<Item = u8>) -> Result<Instruction> {
        let mut raw: Vec<u8> = vec![];
        let mut rd = || -> Result<u8> {
            let b = stream.next().ok_or(DecodeErr::EndOfStream)?;
            raw.push(b);
            Ok(b)
        };
        let b = rd()?;
        let cb = b == 0xCB;
        let def: &InstructionDef = if cb {
            &INSTRUCTIONS_CB[rd()? as usize]
        } else {
            &INSTRUCTIONS[b as usize]
        };

        // Decode immediate values.
        let mut immediate: [ImmediateVal; 2] = [ImmediateVal::None; 2];
        for (i, operand) in def.operands.iter().enumerate() {
            match operand {
                Operand::Immediate8
                | Operand::ImmediateIndirect8
                | Operand::Relative8
                | Operand::SPRelative8 => {
                    immediate[i] = ImmediateVal::Immediate8(rd()?);
                }
                Operand::Immediate16 | Operand::ImmediateIndirect16 => {
                    let mut val: u16 = rd()? as u16;
                    val |= (rd()? as u16) << 8;
                    immediate[i] = ImmediateVal::Immediate16(val);
                }
                _ => {}
            }
        }

        Ok(Instruction {
            def,
            immediate,
            len: raw.len(),
            raw,
        })
    }

    /// Read 8-bit immediate value.
    ///
    /// Returns an error if index is out of bounds or
    /// the requested value is not 8-bit.
    pub fn imm8(&self, idx: usize) -> Result<u8> {
        let immval = self.immediate.get(idx).context("Index out of bounds")?;
        if let ImmediateVal::Immediate8(val) = immval {
            Ok(*val)
        } else {
            bail!("Value not 8-bit")
        }
    }

    /// Read 8-bit immediate value, as signed.
    ///
    /// Returns an error if index is out of bounds or
    /// the requested value is not 8-bit.
    pub fn imms8(&self, idx: usize) -> Result<i8> {
        // TODO fix representation in instruction table and disassembly.

        let immval = self.immediate.get(idx).context("Index out of bounds")?;
        if let ImmediateVal::Immediate8(val) = immval {
            Ok(*val as i8)
        } else {
            bail!("Value not 8-bit")
        }
    }

    /// Read 16-bit immediate value.
    ///
    /// Returns an error if index is out of bounds or
    /// the requested value is not 16-bit.
    pub fn imm16(&self, idx: usize) -> Result<u16> {
        let immval = self.immediate.get(idx).context("Index out of bounds")?;
        if let ImmediateVal::Immediate16(val) = immval {
            Ok(*val)
        } else {
            bail!("Value not 16-bit")
        }
    }

    /// Returns the (first) opcode of the instruction.
    pub fn get_opcode(&self) -> u8 {
        self.raw[0]
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = self.def.mnemonic.to_string();

        // Fill in immediate values.
        for (i, operand) in self.def.operands.iter().enumerate() {
            s = match operand {
                Operand::Immediate8 => {
                    s.replacen("d8", format!("{}", self.immediate[i]).as_str(), 1)
                }
                Operand::ImmediateIndirect8 => {
                    s.replacen("a8", format!("{}", self.immediate[i]).as_str(), 1)
                }
                Operand::Immediate16 => {
                    s.replacen("d16", format!("{}", self.immediate[i]).as_str(), 1)
                }
                Operand::ImmediateIndirect16 => {
                    s.replacen("a16", format!("{}", self.immediate[i]).as_str(), 1)
                }
                Operand::Relative8 | Operand::SPRelative8 => {
                    s.replacen("r8", format!("{}", self.immediate[i]).as_str(), 1)
                }
                _ => s,
            }
        }
        write!(f, "{:02X?} {}", self.raw, s.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instruction_decode() {
        let test = vec![0x00];
        let i = Instruction::decode(&mut test.into_iter()).unwrap();
        assert!(i.def.mnemonic == INSTRUCTIONS[0].mnemonic);
    }

    #[test]
    fn instruction_decode_cb() {
        let test = vec![0xCB, 0x00];
        let i = Instruction::decode(&mut test.into_iter()).unwrap();
        assert!(i.def.mnemonic == INSTRUCTIONS_CB[0].mnemonic);
    }
}
