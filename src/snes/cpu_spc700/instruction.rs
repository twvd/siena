use std::fmt;

use anyhow::Result;
use thiserror::Error;

use super::instruction_table::INSTRUCTION_TABLE;
use super::regs::Register;

/// Instruction operands
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Operand {
    None,
    Implied,
    ImpliedNum(usize),
    Relative,
    Immediate,
    Register(Register),
    DirectPage,
    DirectPageBit(usize),
    DirectPageX,
    DirectPageY,
    Absolute,
    AbsoluteBooleanBit,
    AbsoluteNotBooleanBit,
    AbsoluteXIndexIndirect,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectXAutoInc,
    IndirectY,
    IndirectYIndex,
    XIndexIndirect,
    YIndexIndirect,
}

/// A definition in the instruction (op code) table
pub struct InstructionDef {
    /// String representation
    /// '@' represents an immediate value to be replacee
    pub mnemonic: &'static str,

    /// Operands
    pub operands: [Operand; 2],

    /// Length
    pub len: usize,

    /// Instruction type
    pub instr_type: InstructionType,
}

pub enum InstructionType {
    BMI,
    TCLR1,
    CLR1,
    MOV,
    SETC,
    ADC,
    AND1,
    NOT1,
    STOP,
    SLEEP,
    SETP,
    XCN,
    DI,
    SBC,
    PUSH,
    RET1,
    BBC,
    LSR,
    DAS,
    ADDW,
    ASL,
    SUBW,
    MUL,
    JMP,
    PCALL,
    BBS,
    CBNE,
    DBNZ,
    MOVW,
    DAA,
    DEC,
    POP,
    EOR,
    BNE,
    BVS,
    TCALL,
    BRA,
    BVC,
    DIV,
    BPL,
    BRK,
    OR1,
    EI,
    MOV1,
    DECW,
    ROR,
    OR,
    EOR1,
    BCC,
    NOTC,
    BEQ,
    CLRP,
    CLRV,
    CALL,
    CMP,
    INCW,
    INC,
    TSET1,
    AND,
    CMPW,
    NOP,
    BCS,
    RET,
    CLRC,
    SET1,
    ROL,
}

#[derive(Debug, Error)]
enum DecodeErr {
    #[error("End of instruction stream")]
    EndOfStream,
}

/// A decoded instruction
pub struct Instruction {
    /// Reference to definition in instruction table
    pub def: &'static InstructionDef,

    /// Immediate values
    pub immediate: [u8; 2],

    /// Raw bytes
    pub raw: Vec<u8>,

    /// Instruction length
    pub len: usize,
}

impl Instruction {
    /// Try to decode a single instruction from an iterator.
    pub fn decode(stream: &mut impl Iterator<Item = u8>) -> Result<Instruction> {
        let mut raw: Vec<u8> = vec![];
        let mut rd = || -> Result<u8> {
            let b = stream.next().ok_or(DecodeErr::EndOfStream)?;
            raw.push(b);
            Ok(b)
        };

        let opcode = rd()?;
        let def = &INSTRUCTION_TABLE[opcode as usize];
        let mut args = [0; 4];
        let len = def.len;
        for i in 0..(len - 1) {
            args[i] = rd()?;
        }

        // Transform immediate values
        let mut immediate = [0; 2];
        if def.len > 2 {
            // The immediate values are stored in opposite order of the
            // normal reading order of operands.
            // Flip them around here so they match the operand indices.
            immediate[0] = args[1];
            immediate[1] = args[0];
        } else {
            immediate[0] = args[0];
        }

        Ok(Instruction {
            def,
            immediate,
            raw,
            len,
        })
    }

    /// Read a single 8-bit immediate value.
    pub fn imm8(&self, idx: usize) -> u8 {
        self.immediate[idx]
    }

    /// Read a single 16-bit immediate value.
    pub fn imm16(&self) -> u16 {
        // This is reversed because we saved the immediate values
        // in reverse during decode.
        // The maximum instruction length is 3, so there can only
        // ever be one 16-bit immediate value.
        u16::from_be_bytes(self.immediate)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = self.def.mnemonic.to_string();
        let mut deci = 0;

        while s.contains('@') {
            s = s.replacen('@', format!("${:02X}", self.immediate[deci]).as_str(), 1);
            deci += 1;
        }

        write!(f, "{:02X?} {}", self.raw, s.as_str())
    }
}
