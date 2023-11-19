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
    XIndexAbsolute,
    YIndexAbsolute,
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
