use super::instruction::{InstructionDef, InstructionType, Operand};
use super::regs::Register;

macro_rules! instr {
    ($mnemonic:expr, $typ:expr, $len:expr, $args:expr) => {
        InstructionDef {
            mnemonic: $mnemonic,
            instr_type: $typ,
            len: $len,
            operands: $args,
        }
    };
}

pub const INSTRUCTION_TABLE: [InstructionDef; 256] = [
    // 0x00 - NOP
    // ........, 1 bytes, 2 cycles
    instr!(
        "NOP",
        InstructionType::NOP,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x01 - TCALL 0
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 0",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(0), Operand::None]
    ),
    // 0x02 - SET1  d.0
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.0",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(0), Operand::None]
    ),
    // 0x03 - BBS   d.0, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.0, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(0), Operand::Relative]
    ),
    // 0x04 - OR    A, d
    // N.....Z., 2 bytes, 3 cycles
    instr!(
        "OR    A, d",
        InstructionType::OR,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0x05 - OR    A, !a
    // N.....Z., 3 bytes, 4 cycles
    instr!(
        "OR    A, @",
        InstructionType::OR,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0x06 - OR    A, (X)
    // N.....Z., 1 bytes, 3 cycles
    instr!(
        "OR    A, (X)",
        InstructionType::OR,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0x07 - OR    A, [d+X]
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "OR    A, [d+X]",
        InstructionType::OR,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0x08 - OR    A, #i
    // N.....Z., 2 bytes, 2 cycles
    instr!(
        "OR    A, @",
        InstructionType::OR,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0x09 - OR    dd, ds
    // N.....Z., 3 bytes, 6 cycles
    instr!(
        "OR    dd, ds",
        InstructionType::OR,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0x0A - OR1   C, m.b
    // .......C, 3 bytes, 5 cycles
    instr!(
        "OR1   C, m.b",
        InstructionType::OR1,
        3,
        [Operand::None, Operand::AbsoluteBooleanBit]
    ),
    // 0x0B - ASL   d
    // N.....ZC, 2 bytes, 4 cycles
    instr!(
        "ASL   d",
        InstructionType::ASL,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x0C - ASL   !a
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "ASL   @",
        InstructionType::ASL,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x0D - PUSH  PSW
    // ........, 1 bytes, 4 cycles
    instr!(
        "PUSH  PSW",
        InstructionType::PUSH,
        1,
        [Operand::Register(Register::PSW), Operand::None]
    ),
    // 0x0E - TSET1 !a
    // N.....Z., 3 bytes, 6 cycles
    instr!(
        "TSET1 @",
        InstructionType::TSET1,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x0F - BRK
    // ...1.0.., 1 bytes, 8 cycles
    instr!(
        "BRK",
        InstructionType::BRK,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x10 - BPL   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BPL   @",
        InstructionType::BPL,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0x11 - TCALL 1
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 1",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(1), Operand::None]
    ),
    // 0x12 - CLR1  d.0
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.0",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(0), Operand::None]
    ),
    // 0x13 - BBC   d.0, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.0, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(0), Operand::Relative]
    ),
    // 0x14 - OR    A, d+X
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "OR    A, d+X",
        InstructionType::OR,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0x15 - OR    A, !a+X
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "OR    A, @+X",
        InstructionType::OR,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0x16 - OR    A, !a+Y
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "OR    A, @+Y",
        InstructionType::OR,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0x17 - OR    A, [d]+Y
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "OR    A, [d]+Y",
        InstructionType::OR,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0x18 - OR    d, #i
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "OR    d, @",
        InstructionType::OR,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0x19 - OR    (X), (Y)
    // N.....Z., 1 bytes, 5 cycles
    instr!(
        "OR    (X), (Y)",
        InstructionType::OR,
        1,
        [Operand::IndirectX, Operand::IndirectY]
    ),
    // 0x1A - DECW  d
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "DECW  d",
        InstructionType::DECW,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x1B - ASL   d+X
    // N.....ZC, 2 bytes, 5 cycles
    instr!(
        "ASL   d+X",
        InstructionType::ASL,
        2,
        [Operand::DirectPageX, Operand::None]
    ),
    // 0x1C - ASL   A
    // N.....ZC, 1 bytes, 2 cycles
    instr!(
        "ASL   A",
        InstructionType::ASL,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0x1D - DEC   X
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "DEC   X",
        InstructionType::DEC,
        1,
        [Operand::Register(Register::X), Operand::None]
    ),
    // 0x1E - CMP   X, !a
    // N.....ZC, 3 bytes, 4 cycles
    instr!(
        "CMP   X, @",
        InstructionType::CMP,
        3,
        [Operand::Register(Register::X), Operand::Absolute]
    ),
    // 0x1F - JMP   [!a+X]
    // ........, 3 bytes, 6 cycles
    instr!(
        "JMP   [@+X]",
        InstructionType::JMP,
        3,
        [Operand::AbsoluteXIndexIndirect, Operand::None]
    ),
    // 0x20 - CLRP
    // ..0....., 1 bytes, 2 cycles
    instr!(
        "CLRP",
        InstructionType::CLRP,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x21 - TCALL 2
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 2",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(2), Operand::None]
    ),
    // 0x22 - SET1  d.1
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.1",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(1), Operand::None]
    ),
    // 0x23 - BBS   d.1, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.1, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(1), Operand::Relative]
    ),
    // 0x24 - AND   A, d
    // N.....Z., 2 bytes, 3 cycles
    instr!(
        "AND   A, d",
        InstructionType::AND,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0x25 - AND   A, !a
    // N.....Z., 3 bytes, 4 cycles
    instr!(
        "AND   A, @",
        InstructionType::AND,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0x26 - AND   A, (X)
    // N.....Z., 1 bytes, 3 cycles
    instr!(
        "AND   A, (X)",
        InstructionType::AND,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0x27 - AND   A, [d+X]
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "AND   A, [d+X]",
        InstructionType::AND,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0x28 - AND   A, #i
    // N.....Z., 2 bytes, 2 cycles
    instr!(
        "AND   A, @",
        InstructionType::AND,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0x29 - AND   dd, ds
    // N.....Z., 3 bytes, 6 cycles
    instr!(
        "AND   dd, ds",
        InstructionType::AND,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0x2A - OR1   C, /m.b
    // .......C, 3 bytes, 5 cycles
    instr!(
        "OR1   C, /m.b",
        InstructionType::OR1,
        3,
        [Operand::None, Operand::AbsoluteNotBooleanBit]
    ),
    // 0x2B - ROL   d
    // N.....ZC, 2 bytes, 4 cycles
    instr!(
        "ROL   d",
        InstructionType::ROL,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x2C - ROL   !a
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "ROL   @",
        InstructionType::ROL,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x2D - PUSH  A
    // ........, 1 bytes, 4 cycles
    instr!(
        "PUSH  A",
        InstructionType::PUSH,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0x2E - CBNE  d, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "CBNE  d, @",
        InstructionType::CBNE,
        3,
        [Operand::DirectPage, Operand::Relative]
    ),
    // 0x2F - BRA   r
    // ........, 2 bytes, 4 cycles
    instr!(
        "BRA   @",
        InstructionType::BRA,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0x30 - BMI   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BMI   @",
        InstructionType::BMI,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0x31 - TCALL 3
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 3",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(3), Operand::None]
    ),
    // 0x32 - CLR1  d.1
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.1",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(1), Operand::None]
    ),
    // 0x33 - BBC   d.1, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.1, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(1), Operand::Relative]
    ),
    // 0x34 - AND   A, d+X
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "AND   A, d+X",
        InstructionType::AND,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0x35 - AND   A, !a+X
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "AND   A, @+X",
        InstructionType::AND,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0x36 - AND   A, !a+Y
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "AND   A, @+Y",
        InstructionType::AND,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0x37 - AND   A, [d]+Y
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "AND   A, [d]+Y",
        InstructionType::AND,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0x38 - AND   d, #i
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "AND   d, @",
        InstructionType::AND,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0x39 - AND   (X), (Y)
    // N.....Z., 1 bytes, 5 cycles
    instr!(
        "AND   (X), (Y)",
        InstructionType::AND,
        1,
        [Operand::IndirectX, Operand::IndirectY]
    ),
    // 0x3A - INCW  d
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "INCW  d",
        InstructionType::INCW,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x3B - ROL   d+X
    // N.....ZC, 2 bytes, 5 cycles
    instr!(
        "ROL   d+X",
        InstructionType::ROL,
        2,
        [Operand::DirectPageX, Operand::None]
    ),
    // 0x3C - ROL   A
    // N.....ZC, 1 bytes, 2 cycles
    instr!(
        "ROL   A",
        InstructionType::ROL,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0x3D - INC   X
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "INC   X",
        InstructionType::INC,
        1,
        [Operand::Register(Register::X), Operand::None]
    ),
    // 0x3E - CMP   X, d
    // N.....ZC, 2 bytes, 3 cycles
    instr!(
        "CMP   X, d",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::X), Operand::DirectPage]
    ),
    // 0x3F - CALL  !a
    // ........, 3 bytes, 8 cycles
    instr!(
        "CALL  @",
        InstructionType::CALL,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x40 - SETP
    // ..1....., 1 bytes, 2 cycles
    instr!(
        "SETP",
        InstructionType::SETP,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x41 - TCALL 4
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 4",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(4), Operand::None]
    ),
    // 0x42 - SET1  d.2
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.2",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(2), Operand::None]
    ),
    // 0x43 - BBS   d.2, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.2, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(2), Operand::Relative]
    ),
    // 0x44 - EOR   A, d
    // N.....Z., 2 bytes, 3 cycles
    instr!(
        "EOR   A, d",
        InstructionType::EOR,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0x45 - EOR   A, !a
    // N.....Z., 3 bytes, 4 cycles
    instr!(
        "EOR   A, @",
        InstructionType::EOR,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0x46 - EOR   A, (X)
    // N.....Z., 1 bytes, 3 cycles
    instr!(
        "EOR   A, (X)",
        InstructionType::EOR,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0x47 - EOR   A, [d+X]
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "EOR   A, [d+X]",
        InstructionType::EOR,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0x48 - EOR   A, #i
    // N.....Z., 2 bytes, 2 cycles
    instr!(
        "EOR   A, @",
        InstructionType::EOR,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0x49 - EOR   dd, ds
    // N.....Z., 3 bytes, 6 cycles
    instr!(
        "EOR   dd, ds",
        InstructionType::EOR,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0x4A - AND1  C, m.b
    // .......C, 3 bytes, 4 cycles
    instr!(
        "AND1  C, m.b",
        InstructionType::AND1,
        3,
        [Operand::None, Operand::AbsoluteBooleanBit]
    ),
    // 0x4B - LSR   d
    // N.....ZC, 2 bytes, 4 cycles
    instr!(
        "LSR   d",
        InstructionType::LSR,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x4C - LSR   !a
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "LSR   @",
        InstructionType::LSR,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x4D - PUSH  X
    // ........, 1 bytes, 4 cycles
    instr!(
        "PUSH  X",
        InstructionType::PUSH,
        1,
        [Operand::Register(Register::X), Operand::None]
    ),
    // 0x4E - TCLR1 !a
    // N.....Z., 3 bytes, 6 cycles
    instr!(
        "TCLR1 @",
        InstructionType::TCLR1,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x4F - PCALL u
    // ........, 2 bytes, 6 cycles
    instr!(
        "PCALL u",
        InstructionType::PCALL,
        2,
        [Operand::Immediate, Operand::None]
    ),
    // 0x50 - BVC   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BVC   @",
        InstructionType::BVC,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0x51 - TCALL 5
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 5",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(5), Operand::None]
    ),
    // 0x52 - CLR1  d.2
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.2",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(2), Operand::None]
    ),
    // 0x53 - BBC   d.2, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.2, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(2), Operand::Relative]
    ),
    // 0x54 - EOR   A, d+X
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "EOR   A, d+X",
        InstructionType::EOR,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0x55 - EOR   A, !a+X
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "EOR   A, @+X",
        InstructionType::EOR,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0x56 - EOR   A, !a+Y
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "EOR   A, @+Y",
        InstructionType::EOR,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0x57 - EOR   A, [d]+Y
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "EOR   A, [d]+Y",
        InstructionType::EOR,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0x58 - EOR   d, #i
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "EOR   d, @",
        InstructionType::EOR,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0x59 - EOR   (X), (Y)
    // N.....Z., 1 bytes, 5 cycles
    instr!(
        "EOR   (X), (Y)",
        InstructionType::EOR,
        1,
        [Operand::IndirectX, Operand::IndirectY]
    ),
    // 0x5A - CMPW  YA, d
    // N.....ZC, 2 bytes, 4 cycles
    instr!(
        "CMPW  YA, d",
        InstructionType::CMPW,
        2,
        [Operand::None, Operand::DirectPage]
    ),
    // 0x5B - LSR   d+X
    // N.....ZC, 2 bytes, 5 cycles
    instr!(
        "LSR   d+X",
        InstructionType::LSR,
        2,
        [Operand::DirectPageX, Operand::None]
    ),
    // 0x5C - LSR   A
    // N.....ZC, 1 bytes, 2 cycles
    instr!(
        "LSR   A",
        InstructionType::LSR,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0x5D - MOV   X, A
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "MOV   X, A",
        InstructionType::MOV,
        1,
        [
            Operand::Register(Register::X),
            Operand::Register(Register::A)
        ]
    ),
    // 0x5E - CMP   Y, !a
    // N.....ZC, 3 bytes, 4 cycles
    instr!(
        "CMP   Y, @",
        InstructionType::CMP,
        3,
        [Operand::Register(Register::Y), Operand::Absolute]
    ),
    // 0x5F - JMP   !a
    // ........, 3 bytes, 3 cycles
    instr!(
        "JMP   @",
        InstructionType::JMP,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x60 - CLRC
    // .......0, 1 bytes, 2 cycles
    instr!(
        "CLRC",
        InstructionType::CLRC,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x61 - TCALL 6
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 6",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(6), Operand::None]
    ),
    // 0x62 - SET1  d.3
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.3",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(3), Operand::None]
    ),
    // 0x63 - BBS   d.3, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.3, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(3), Operand::Relative]
    ),
    // 0x64 - CMP   A, d
    // N.....ZC, 2 bytes, 3 cycles
    instr!(
        "CMP   A, d",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0x65 - CMP   A, !a
    // N.....ZC, 3 bytes, 4 cycles
    instr!(
        "CMP   A, @",
        InstructionType::CMP,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0x66 - CMP   A, (X)
    // N.....ZC, 1 bytes, 3 cycles
    instr!(
        "CMP   A, (X)",
        InstructionType::CMP,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0x67 - CMP   A, [d+X]
    // N.....ZC, 2 bytes, 6 cycles
    instr!(
        "CMP   A, [d+X]",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0x68 - CMP   A, #i
    // N.....ZC, 2 bytes, 2 cycles
    instr!(
        "CMP   A, @",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0x69 - CMP   dd, ds
    // N.....ZC, 3 bytes, 6 cycles
    instr!(
        "CMP   dd, ds",
        InstructionType::CMP,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0x6A - AND1  C, /m.b
    // .......C, 3 bytes, 4 cycles
    instr!(
        "AND1  C, /m.b",
        InstructionType::AND1,
        3,
        [Operand::None, Operand::AbsoluteNotBooleanBit]
    ),
    // 0x6B - ROR   d
    // N.....ZC, 2 bytes, 4 cycles
    instr!(
        "ROR   d",
        InstructionType::ROR,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x6C - ROR   !a
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "ROR   @",
        InstructionType::ROR,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x6D - PUSH  Y
    // ........, 1 bytes, 4 cycles
    instr!(
        "PUSH  Y",
        InstructionType::PUSH,
        1,
        [Operand::Register(Register::Y), Operand::None]
    ),
    // 0x6E - DBNZ  d, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "DBNZ  d, @",
        InstructionType::DBNZ,
        3,
        [Operand::DirectPage, Operand::Relative]
    ),
    // 0x6F - RET
    // ........, 1 bytes, 5 cycles
    instr!(
        "RET",
        InstructionType::RET,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x70 - BVS   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BVS   @",
        InstructionType::BVS,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0x71 - TCALL 7
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 7",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(7), Operand::None]
    ),
    // 0x72 - CLR1  d.3
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.3",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(3), Operand::None]
    ),
    // 0x73 - BBC   d.3, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.3, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(3), Operand::Relative]
    ),
    // 0x74 - CMP   A, d+X
    // N.....ZC, 2 bytes, 4 cycles
    instr!(
        "CMP   A, d+X",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0x75 - CMP   A, !a+X
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "CMP   A, @+X",
        InstructionType::CMP,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0x76 - CMP   A, !a+Y
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "CMP   A, @+Y",
        InstructionType::CMP,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0x77 - CMP   A, [d]+Y
    // N.....ZC, 2 bytes, 6 cycles
    instr!(
        "CMP   A, [d]+Y",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0x78 - CMP   d, #i
    // N.....ZC, 3 bytes, 5 cycles
    instr!(
        "CMP   d, @",
        InstructionType::CMP,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0x79 - CMP   (X), (Y)
    // N.....ZC, 1 bytes, 5 cycles
    instr!(
        "CMP   (X), (Y)",
        InstructionType::CMP,
        1,
        [Operand::IndirectX, Operand::IndirectY]
    ),
    // 0x7A - ADDW  YA, d
    // NV..H.ZC, 2 bytes, 5 cycles
    instr!(
        "ADDW  YA, d",
        InstructionType::ADDW,
        2,
        [Operand::None, Operand::DirectPage]
    ),
    // 0x7B - ROR   d+X
    // N.....ZC, 2 bytes, 5 cycles
    instr!(
        "ROR   d+X",
        InstructionType::ROR,
        2,
        [Operand::DirectPageX, Operand::None]
    ),
    // 0x7C - ROR   A
    // N.....ZC, 1 bytes, 2 cycles
    instr!(
        "ROR   A",
        InstructionType::ROR,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0x7D - MOV   A, X
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "MOV   A, X",
        InstructionType::MOV,
        1,
        [
            Operand::Register(Register::A),
            Operand::Register(Register::X)
        ]
    ),
    // 0x7E - CMP   Y, d
    // N.....ZC, 2 bytes, 3 cycles
    instr!(
        "CMP   Y, d",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::Y), Operand::DirectPage]
    ),
    // 0x7F - RET1
    // NVPBHIZC, 1 bytes, 6 cycles
    instr!(
        "RET1",
        InstructionType::RET1,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x80 - SETC
    // .......1, 1 bytes, 2 cycles
    instr!(
        "SETC",
        InstructionType::SETC,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0x81 - TCALL 8
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 8",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(8), Operand::None]
    ),
    // 0x82 - SET1  d.4
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.4",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(4), Operand::None]
    ),
    // 0x83 - BBS   d.4, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.4, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(4), Operand::Relative]
    ),
    // 0x84 - ADC   A, d
    // NV..H.ZC, 2 bytes, 3 cycles
    instr!(
        "ADC   A, d",
        InstructionType::ADC,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0x85 - ADC   A, !a
    // NV..H.ZC, 3 bytes, 4 cycles
    instr!(
        "ADC   A, @",
        InstructionType::ADC,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0x86 - ADC   A, (X)
    // NV..H.ZC, 1 bytes, 3 cycles
    instr!(
        "ADC   A, (X)",
        InstructionType::ADC,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0x87 - ADC   A, [d+X]
    // NV..H.ZC, 2 bytes, 6 cycles
    instr!(
        "ADC   A, [d+X]",
        InstructionType::ADC,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0x88 - ADC   A, #i
    // NV..H.ZC, 2 bytes, 2 cycles
    instr!(
        "ADC   A, @",
        InstructionType::ADC,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0x89 - ADC   dd, ds
    // NV..H.ZC, 3 bytes, 6 cycles
    instr!(
        "ADC   dd, ds",
        InstructionType::ADC,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0x8A - EOR1  C, m.b
    // .......C, 3 bytes, 5 cycles
    instr!(
        "EOR1  C, m.b",
        InstructionType::EOR1,
        3,
        [Operand::None, Operand::AbsoluteBooleanBit]
    ),
    // 0x8B - DEC   d
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "DEC   d",
        InstructionType::DEC,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0x8C - DEC   !a
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "DEC   @",
        InstructionType::DEC,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0x8D - MOV   Y, #i
    // N.....Z., 2 bytes, 2 cycles
    instr!(
        "MOV   Y, @",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::Y), Operand::Immediate]
    ),
    // 0x8E - POP   PSW
    // NVPBHIZC, 1 bytes, 4 cycles
    instr!(
        "POP   PSW",
        InstructionType::POP,
        1,
        [Operand::Register(Register::PSW), Operand::None]
    ),
    // 0x8F - MOV   d, #i
    // ........, 3 bytes, 5 cycles
    instr!(
        "MOV   d, @",
        InstructionType::MOV,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0x90 - BCC   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BCC   @",
        InstructionType::BCC,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0x91 - TCALL 9
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 9",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(9), Operand::None]
    ),
    // 0x92 - CLR1  d.4
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.4",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(4), Operand::None]
    ),
    // 0x93 - BBC   d.4, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.4, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(4), Operand::Relative]
    ),
    // 0x94 - ADC   A, d+X
    // NV..H.ZC, 2 bytes, 4 cycles
    instr!(
        "ADC   A, d+X",
        InstructionType::ADC,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0x95 - ADC   A, !a+X
    // NV..H.ZC, 3 bytes, 5 cycles
    instr!(
        "ADC   A, @+X",
        InstructionType::ADC,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0x96 - ADC   A, !a+Y
    // NV..H.ZC, 3 bytes, 5 cycles
    instr!(
        "ADC   A, @+Y",
        InstructionType::ADC,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0x97 - ADC   A, [d]+Y
    // NV..H.ZC, 2 bytes, 6 cycles
    instr!(
        "ADC   A, [d]+Y",
        InstructionType::ADC,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0x98 - ADC   d, #i
    // NV..H.ZC, 3 bytes, 5 cycles
    instr!(
        "ADC   d, @",
        InstructionType::ADC,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0x99 - ADC   (X), (Y)
    // NV..H.ZC, 1 bytes, 5 cycles
    instr!(
        "ADC   (X), (Y)",
        InstructionType::ADC,
        1,
        [Operand::IndirectX, Operand::IndirectY]
    ),
    // 0x9A - SUBW  YA, d
    // NV..H.ZC, 2 bytes, 5 cycles
    instr!(
        "SUBW  YA, d",
        InstructionType::SUBW,
        2,
        [Operand::None, Operand::DirectPage]
    ),
    // 0x9B - DEC   d+X
    // N.....Z., 2 bytes, 5 cycles
    instr!(
        "DEC   d+X",
        InstructionType::DEC,
        2,
        [Operand::DirectPageX, Operand::None]
    ),
    // 0x9C - DEC   A
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "DEC   A",
        InstructionType::DEC,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0x9D - MOV   X, SP
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "MOV   X, SP",
        InstructionType::MOV,
        1,
        [
            Operand::Register(Register::X),
            Operand::Register(Register::SP)
        ]
    ),
    // 0x9E - DIV   YA, X
    // NV..H.Z., 1 bytes, 12 cycles
    instr!(
        "DIV   YA, X",
        InstructionType::DIV,
        1,
        [Operand::None, Operand::Register(Register::X)]
    ),
    // 0x9F - XCN   A
    // N.....Z., 1 bytes, 5 cycles
    instr!(
        "XCN   A",
        InstructionType::XCN,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0xA0 - EI
    // .....1.., 1 bytes, 3 cycles
    instr!(
        "EI",
        InstructionType::EI,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0xA1 - TCALL 10
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 10",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(10), Operand::None]
    ),
    // 0xA2 - SET1  d.5
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.5",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(5), Operand::None]
    ),
    // 0xA3 - BBS   d.5, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.5, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(5), Operand::Relative]
    ),
    // 0xA4 - SBC   A, d
    // NV..H.ZC, 2 bytes, 3 cycles
    instr!(
        "SBC   A, d",
        InstructionType::SBC,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0xA5 - SBC   A, !a
    // NV..H.ZC, 3 bytes, 4 cycles
    instr!(
        "SBC   A, @",
        InstructionType::SBC,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0xA6 - SBC   A, (X)
    // NV..H.ZC, 1 bytes, 3 cycles
    instr!(
        "SBC   A, (X)",
        InstructionType::SBC,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0xA7 - SBC   A, [d+X]
    // NV..H.ZC, 2 bytes, 6 cycles
    instr!(
        "SBC   A, [d+X]",
        InstructionType::SBC,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0xA8 - SBC   A, #i
    // NV..H.ZC, 2 bytes, 2 cycles
    instr!(
        "SBC   A, @",
        InstructionType::SBC,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0xA9 - SBC   dd, ds
    // NV..H.ZC, 3 bytes, 6 cycles
    instr!(
        "SBC   dd, ds",
        InstructionType::SBC,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0xAA - MOV1  C, m.b
    // .......C, 3 bytes, 4 cycles
    instr!(
        "MOV1  C, m.b",
        InstructionType::MOV1,
        3,
        [Operand::None, Operand::AbsoluteBooleanBit]
    ),
    // 0xAB - INC   d
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "INC   d",
        InstructionType::INC,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0xAC - INC   !a
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "INC   @",
        InstructionType::INC,
        3,
        [Operand::Absolute, Operand::None]
    ),
    // 0xAD - CMP   Y, #i
    // N.....ZC, 2 bytes, 2 cycles
    instr!(
        "CMP   Y, @",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::Y), Operand::Immediate]
    ),
    // 0xAE - POP   A
    // ........, 1 bytes, 4 cycles
    instr!(
        "POP   A",
        InstructionType::POP,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0xAF - MOV   (X)+, A
    // ........, 1 bytes, 4 cycles
    instr!(
        "MOV   (X)+, A",
        InstructionType::MOV,
        1,
        [Operand::IndirectXAutoInc, Operand::Register(Register::A)]
    ),
    // 0xB0 - BCS   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BCS   @",
        InstructionType::BCS,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0xB1 - TCALL 11
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 11",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(11), Operand::None]
    ),
    // 0xB2 - CLR1  d.5
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.5",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(5), Operand::None]
    ),
    // 0xB3 - BBC   d.5, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.5, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(5), Operand::Relative]
    ),
    // 0xB4 - SBC   A, d+X
    // NV..H.ZC, 2 bytes, 4 cycles
    instr!(
        "SBC   A, d+X",
        InstructionType::SBC,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0xB5 - SBC   A, !a+X
    // NV..H.ZC, 3 bytes, 5 cycles
    instr!(
        "SBC   A, @+X",
        InstructionType::SBC,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0xB6 - SBC   A, !a+Y
    // NV..H.ZC, 3 bytes, 5 cycles
    instr!(
        "SBC   A, @+Y",
        InstructionType::SBC,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0xB7 - SBC   A, [d]+Y
    // NV..H.ZC, 2 bytes, 6 cycles
    instr!(
        "SBC   A, [d]+Y",
        InstructionType::SBC,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0xB8 - SBC   d, #i
    // NV..H.ZC, 3 bytes, 5 cycles
    instr!(
        "SBC   d, @",
        InstructionType::SBC,
        3,
        [Operand::DirectPage, Operand::Immediate]
    ),
    // 0xB9 - SBC   (X), (Y)
    // NV..H.ZC, 1 bytes, 5 cycles
    instr!(
        "SBC   (X), (Y)",
        InstructionType::SBC,
        1,
        [Operand::IndirectX, Operand::IndirectY]
    ),
    // 0xBA - MOVW  YA, d
    // N.....Z., 2 bytes, 5 cycles
    instr!(
        "MOVW  YA, d",
        InstructionType::MOVW,
        2,
        [Operand::None, Operand::DirectPage]
    ),
    // 0xBB - INC   d+X
    // N.....Z., 2 bytes, 5 cycles
    instr!(
        "INC   d+X",
        InstructionType::INC,
        2,
        [Operand::DirectPageX, Operand::None]
    ),
    // 0xBC - INC   A
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "INC   A",
        InstructionType::INC,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0xBD - MOV   SP, X
    // ........, 1 bytes, 2 cycles
    instr!(
        "MOV   SP, X",
        InstructionType::MOV,
        1,
        [
            Operand::Register(Register::SP),
            Operand::Register(Register::X)
        ]
    ),
    // 0xBE - DAS   A
    // N.....ZC, 1 bytes, 3 cycles
    instr!(
        "DAS   A",
        InstructionType::DAS,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0xBF - MOV   A, (X)+
    // N.....Z., 1 bytes, 4 cycles
    instr!(
        "MOV   A, (X)+",
        InstructionType::MOV,
        1,
        [Operand::Register(Register::A), Operand::IndirectXAutoInc]
    ),
    // 0xC0 - DI
    // .....0.., 1 bytes, 3 cycles
    instr!(
        "DI",
        InstructionType::DI,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0xC1 - TCALL 12
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 12",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(12), Operand::None]
    ),
    // 0xC2 - SET1  d.6
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.6",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(6), Operand::None]
    ),
    // 0xC3 - BBS   d.6, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.6, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(6), Operand::Relative]
    ),
    // 0xC4 - MOV   d, A
    // ........, 2 bytes, 4 cycles
    instr!(
        "MOV   d, A",
        InstructionType::MOV,
        2,
        [Operand::DirectPage, Operand::Register(Register::A)]
    ),
    // 0xC5 - MOV   !a, A
    // ........, 3 bytes, 5 cycles
    instr!(
        "MOV   @, A",
        InstructionType::MOV,
        3,
        [Operand::Absolute, Operand::Register(Register::A)]
    ),
    // 0xC6 - MOV   (X), A
    // ........, 1 bytes, 4 cycles
    instr!(
        "MOV   (X), A",
        InstructionType::MOV,
        1,
        [Operand::IndirectX, Operand::Register(Register::A)]
    ),
    // 0xC7 - MOV   [d+X], A
    // ........, 2 bytes, 7 cycles
    instr!(
        "MOV   [d+X], A",
        InstructionType::MOV,
        2,
        [Operand::XIndexIndirect, Operand::Register(Register::A)]
    ),
    // 0xC8 - CMP   X, #i
    // N.....ZC, 2 bytes, 2 cycles
    instr!(
        "CMP   X, @",
        InstructionType::CMP,
        2,
        [Operand::Register(Register::X), Operand::Immediate]
    ),
    // 0xC9 - MOV   !a, X
    // ........, 3 bytes, 5 cycles
    instr!(
        "MOV   @, X",
        InstructionType::MOV,
        3,
        [Operand::Absolute, Operand::Register(Register::X)]
    ),
    // 0xCA - MOV1  m.b, C
    // ........, 3 bytes, 6 cycles
    instr!(
        "MOV1  m.b, C",
        InstructionType::MOV1,
        3,
        [Operand::AbsoluteBooleanBit, Operand::None]
    ),
    // 0xCB - MOV   d, Y
    // ........, 2 bytes, 4 cycles
    instr!(
        "MOV   d, Y",
        InstructionType::MOV,
        2,
        [Operand::DirectPage, Operand::Register(Register::Y)]
    ),
    // 0xCC - MOV   !a, Y
    // ........, 3 bytes, 5 cycles
    instr!(
        "MOV   @, Y",
        InstructionType::MOV,
        3,
        [Operand::Absolute, Operand::Register(Register::Y)]
    ),
    // 0xCD - MOV   X, #i
    // N.....Z., 2 bytes, 2 cycles
    instr!(
        "MOV   X, @",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::X), Operand::Immediate]
    ),
    // 0xCE - POP   X
    // ........, 1 bytes, 4 cycles
    instr!(
        "POP   X",
        InstructionType::POP,
        1,
        [Operand::Register(Register::X), Operand::None]
    ),
    // 0xCF - MUL   YA
    // N.....Z., 1 bytes, 9 cycles
    instr!(
        "MUL   YA",
        InstructionType::MUL,
        1,
        [Operand::None, Operand::None]
    ),
    // 0xD0 - BNE   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BNE   @",
        InstructionType::BNE,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0xD1 - TCALL 13
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 13",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(13), Operand::None]
    ),
    // 0xD2 - CLR1  d.6
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.6",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(6), Operand::None]
    ),
    // 0xD3 - BBC   d.6, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.6, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(6), Operand::Relative]
    ),
    // 0xD4 - MOV   d+X, A
    // ........, 2 bytes, 5 cycles
    instr!(
        "MOV   d+X, A",
        InstructionType::MOV,
        2,
        [Operand::DirectPageX, Operand::Register(Register::A)]
    ),
    // 0xD5 - MOV   !a+X, A
    // ........, 3 bytes, 6 cycles
    instr!(
        "MOV   @+X, A",
        InstructionType::MOV,
        3,
        [Operand::XIndexAbsolute, Operand::Register(Register::A)]
    ),
    // 0xD6 - MOV   !a+Y, A
    // ........, 3 bytes, 6 cycles
    instr!(
        "MOV   @+Y, A",
        InstructionType::MOV,
        3,
        [Operand::YIndexAbsolute, Operand::Register(Register::A)]
    ),
    // 0xD7 - MOV   [d]+Y, A
    // ........, 2 bytes, 7 cycles
    instr!(
        "MOV   [d]+Y, A",
        InstructionType::MOV,
        2,
        [Operand::IndirectYIndex, Operand::Register(Register::A)]
    ),
    // 0xD8 - MOV   d, X
    // ........, 2 bytes, 4 cycles
    instr!(
        "MOV   d, X",
        InstructionType::MOV,
        2,
        [Operand::DirectPage, Operand::Register(Register::X)]
    ),
    // 0xD9 - MOV   d+Y, X
    // ........, 2 bytes, 5 cycles
    instr!(
        "MOV   d+Y, X",
        InstructionType::MOV,
        2,
        [Operand::DirectPageY, Operand::Register(Register::X)]
    ),
    // 0xDA - MOVW  d, YA
    // ........, 2 bytes, 5 cycles
    instr!(
        "MOVW  d, YA",
        InstructionType::MOVW,
        2,
        [Operand::DirectPage, Operand::None]
    ),
    // 0xDB - MOV   d+X, Y
    // ........, 2 bytes, 5 cycles
    instr!(
        "MOV   d+X, Y",
        InstructionType::MOV,
        2,
        [Operand::DirectPageX, Operand::Register(Register::Y)]
    ),
    // 0xDC - DEC   Y
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "DEC   Y",
        InstructionType::DEC,
        1,
        [Operand::Register(Register::Y), Operand::None]
    ),
    // 0xDD - MOV   A, Y
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "MOV   A, Y",
        InstructionType::MOV,
        1,
        [
            Operand::Register(Register::A),
            Operand::Register(Register::Y)
        ]
    ),
    // 0xDE - CBNE  d+X, r
    // ........, 3 bytes, 6/8 cycles
    instr!(
        "CBNE  d+X, @",
        InstructionType::CBNE,
        3,
        [Operand::DirectPageX, Operand::Relative]
    ),
    // 0xDF - DAA   A
    // N.....ZC, 1 bytes, 3 cycles
    instr!(
        "DAA   A",
        InstructionType::DAA,
        1,
        [Operand::Register(Register::A), Operand::None]
    ),
    // 0xE0 - CLRV
    // .0..0..., 1 bytes, 2 cycles
    instr!(
        "CLRV",
        InstructionType::CLRV,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0xE1 - TCALL 14
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 14",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(14), Operand::None]
    ),
    // 0xE2 - SET1  d.7
    // ........, 2 bytes, 4 cycles
    instr!(
        "SET1  d.7",
        InstructionType::SET1,
        2,
        [Operand::DirectPageBit(7), Operand::None]
    ),
    // 0xE3 - BBS   d.7, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBS   d.7, @",
        InstructionType::BBS,
        3,
        [Operand::DirectPageBit(7), Operand::Relative]
    ),
    // 0xE4 - MOV   A, d
    // N.....Z., 2 bytes, 3 cycles
    instr!(
        "MOV   A, d",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::A), Operand::DirectPage]
    ),
    // 0xE5 - MOV   A, !a
    // N.....Z., 3 bytes, 4 cycles
    instr!(
        "MOV   A, @",
        InstructionType::MOV,
        3,
        [Operand::Register(Register::A), Operand::Absolute]
    ),
    // 0xE6 - MOV   A, (X)
    // N.....Z., 1 bytes, 3 cycles
    instr!(
        "MOV   A, (X)",
        InstructionType::MOV,
        1,
        [Operand::Register(Register::A), Operand::IndirectX]
    ),
    // 0xE7 - MOV   A, [d+X]
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "MOV   A, [d+X]",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::A), Operand::XIndexIndirect]
    ),
    // 0xE8 - MOV   A, #i
    // N.....Z., 2 bytes, 2 cycles
    instr!(
        "MOV   A, @",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::A), Operand::Immediate]
    ),
    // 0xE9 - MOV   X, !a
    // N.....Z., 3 bytes, 4 cycles
    instr!(
        "MOV   X, @",
        InstructionType::MOV,
        3,
        [Operand::Register(Register::X), Operand::Absolute]
    ),
    // 0xEA - NOT1  m.b
    // ........, 3 bytes, 5 cycles
    instr!(
        "NOT1  m.b",
        InstructionType::NOT1,
        3,
        [Operand::AbsoluteBooleanBit, Operand::None]
    ),
    // 0xEB - MOV   Y, d
    // N.....Z., 2 bytes, 3 cycles
    instr!(
        "MOV   Y, d",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::Y), Operand::DirectPage]
    ),
    // 0xEC - MOV   Y, !a
    // N.....Z., 3 bytes, 4 cycles
    instr!(
        "MOV   Y, @",
        InstructionType::MOV,
        3,
        [Operand::Register(Register::Y), Operand::Absolute]
    ),
    // 0xED - NOTC
    // .......C, 1 bytes, 3 cycles
    instr!(
        "NOTC",
        InstructionType::NOTC,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0xEE - POP   Y
    // ........, 1 bytes, 4 cycles
    instr!(
        "POP   Y",
        InstructionType::POP,
        1,
        [Operand::Register(Register::Y), Operand::None]
    ),
    // 0xEF - SLEEP
    // ........, 1 bytes, ? cycles
    instr!(
        "SLEEP",
        InstructionType::SLEEP,
        1,
        [Operand::Implied, Operand::None]
    ),
    // 0xF0 - BEQ   r
    // ........, 2 bytes, 2/4 cycles
    instr!(
        "BEQ   @",
        InstructionType::BEQ,
        2,
        [Operand::Relative, Operand::None]
    ),
    // 0xF1 - TCALL 15
    // ........, 1 bytes, 8 cycles
    instr!(
        "TCALL 15",
        InstructionType::TCALL,
        1,
        [Operand::ImpliedNum(15), Operand::None]
    ),
    // 0xF2 - CLR1  d.7
    // ........, 2 bytes, 4 cycles
    instr!(
        "CLR1  d.7",
        InstructionType::CLR1,
        2,
        [Operand::DirectPageBit(7), Operand::None]
    ),
    // 0xF3 - BBC   d.7, r
    // ........, 3 bytes, 5/7 cycles
    instr!(
        "BBC   d.7, @",
        InstructionType::BBC,
        3,
        [Operand::DirectPageBit(7), Operand::Relative]
    ),
    // 0xF4 - MOV   A, d+X
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "MOV   A, d+X",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::A), Operand::DirectPageX]
    ),
    // 0xF5 - MOV   A, !a+X
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "MOV   A, @+X",
        InstructionType::MOV,
        3,
        [Operand::Register(Register::A), Operand::XIndexAbsolute]
    ),
    // 0xF6 - MOV   A, !a+Y
    // N.....Z., 3 bytes, 5 cycles
    instr!(
        "MOV   A, @+Y",
        InstructionType::MOV,
        3,
        [Operand::Register(Register::A), Operand::YIndexAbsolute]
    ),
    // 0xF7 - MOV   A, [d]+Y
    // N.....Z., 2 bytes, 6 cycles
    instr!(
        "MOV   A, [d]+Y",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::A), Operand::IndirectYIndex]
    ),
    // 0xF8 - MOV   X, d
    // N.....Z., 2 bytes, 3 cycles
    instr!(
        "MOV   X, d",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::X), Operand::DirectPage]
    ),
    // 0xF9 - MOV   X, d+Y
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "MOV   X, d+Y",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::X), Operand::DirectPageY]
    ),
    // 0xFA - MOV   dd, ds
    // ........, 3 bytes, 5 cycles
    instr!(
        "MOV   dd, ds",
        InstructionType::MOV,
        3,
        [Operand::DirectPage, Operand::DirectPage]
    ),
    // 0xFB - MOV   Y, d+X
    // N.....Z., 2 bytes, 4 cycles
    instr!(
        "MOV   Y, d+X",
        InstructionType::MOV,
        2,
        [Operand::Register(Register::Y), Operand::DirectPageX]
    ),
    // 0xFC - INC   Y
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "INC   Y",
        InstructionType::INC,
        1,
        [Operand::Register(Register::Y), Operand::None]
    ),
    // 0xFD - MOV   Y, A
    // N.....Z., 1 bytes, 2 cycles
    instr!(
        "MOV   Y, A",
        InstructionType::MOV,
        1,
        [
            Operand::Register(Register::Y),
            Operand::Register(Register::A)
        ]
    ),
    // 0xFE - DBNZ  Y, r
    // ........, 2 bytes, 4/6 cycles
    instr!(
        "DBNZ  Y, @",
        InstructionType::DBNZ,
        2,
        [Operand::Register(Register::Y), Operand::Relative]
    ),
    // 0xFF - STOP
    // ........, 1 bytes, ? cycles
    instr!(
        "STOP",
        InstructionType::STOP,
        1,
        [Operand::Implied, Operand::None]
    ),
];
