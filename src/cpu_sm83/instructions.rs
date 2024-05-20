use super::instruction::{InstructionDef, Operand};
use super::regs::Register;

/// Instruction type groups
#[allow(non_camel_case_types)]
pub enum InstructionType {
    NOP,
    LD,
    INC16,
    INC8,
    DEC16,
    DEC8,
    CCF,
    HALT,
    SET,
    RES,
    RLCA,
    ADD16,
    ADD,
    SCF,
    ADC,
    RST,
    RRCA,
    STOP,
    RLA,
    JR,
    JR_NZ,
    RRA,
    DAA,
    SBC,
    SUB,
    DI,
    EI,
    INVALID,
    CP,
    RRC,
    RLC,
    RR,
    RL,
    SLA,
    SRA,
    SWAP,
    SRL,
    BIT,
    JRZ,
    CPL,
    JRNC,
    JRC,
    AND,
    OR,
    ADD_SP,
    XOR,
    PUSH,
    POP,
    RET_NC,
    JP_NC,
    JP_C,
    RET_NZ,
    RET_Z,
    RET,
    RETI,
    JP_NZ,
    JP_Z,
    JP,
    CALL_NZ,
    CALL_NC,
    CALL_C,
    RET_C,
    CALL_Z,
    CALL,
}

/// Base instruction table, parsed from https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
pub const INSTRUCTIONS: [InstructionDef; 256] = [
    // NOP (1), - - - -
    InstructionDef {
        mnemonic: "NOP",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::NOP,
    },
    // LD BC,d16 (3), - - - -
    InstructionDef {
        mnemonic: "LD BC,d16",
        operands: [Operand::Register(Register::BC), Operand::Immediate16],
        len: 3,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // LD (BC),A (1), - - - -
    InstructionDef {
        mnemonic: "LD (BC),A",
        operands: [
            Operand::RegisterIndirect(Register::BC),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // INC BC (1), - - - -
    InstructionDef {
        mnemonic: "INC BC",
        operands: [Operand::Register(Register::BC), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::INC16,
    },
    // INC B (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC B (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD B,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD B,d8",
        operands: [Operand::Register(Register::B), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // RLCA (1), 0 0 0 C
    InstructionDef {
        mnemonic: "RLCA",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::RLCA,
    },
    // LD (a16),SP (3), - - - -
    InstructionDef {
        mnemonic: "LD (a16),SP",
        operands: [
            Operand::ImmediateIndirect16,
            Operand::Register(Register::SP),
        ],
        len: 3,
        cycles: [20, 20],
        instr_type: InstructionType::LD,
    },
    // ADD HL,BC (1), - 0 H C
    InstructionDef {
        mnemonic: "ADD HL,BC",
        operands: [
            Operand::Register(Register::HL),
            Operand::Register(Register::BC),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::ADD16,
    },
    // LD A,(BC) (1), - - - -
    InstructionDef {
        mnemonic: "LD A,(BC)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::BC),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // DEC BC (1), - - - -
    InstructionDef {
        mnemonic: "DEC BC",
        operands: [Operand::Register(Register::BC), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::DEC16,
    },
    // INC C (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC C (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD C,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD C,d8",
        operands: [Operand::Register(Register::C), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // RRCA (1), 0 0 0 C
    InstructionDef {
        mnemonic: "RRCA",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::RRCA,
    },
    // STOP 0 (2), - - - -
    InstructionDef {
        mnemonic: "STOP 0",
        operands: [Operand::Immediate8, Operand::None],
        len: 2,
        // 'Condition not met' cycle cost is for CGB speed switch
        cycles: [8, 2050],
        instr_type: InstructionType::STOP,
    },
    // LD DE,d16 (3), - - - -
    InstructionDef {
        mnemonic: "LD DE,d16",
        operands: [Operand::Register(Register::DE), Operand::Immediate16],
        len: 3,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // LD (DE),A (1), - - - -
    InstructionDef {
        mnemonic: "LD (DE),A",
        operands: [
            Operand::RegisterIndirect(Register::DE),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // INC DE (1), - - - -
    InstructionDef {
        mnemonic: "INC DE",
        operands: [Operand::Register(Register::DE), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::INC16,
    },
    // INC D (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC D (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD D,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD D,d8",
        operands: [Operand::Register(Register::D), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // RLA (1), 0 0 0 C
    InstructionDef {
        mnemonic: "RLA",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::RLA,
    },
    // JR r8 (2), - - - -
    InstructionDef {
        mnemonic: "JR r8",
        operands: [Operand::Relative8, Operand::None],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::JR,
    },
    // ADD HL,DE (1), - 0 H C
    InstructionDef {
        mnemonic: "ADD HL,DE",
        operands: [
            Operand::Register(Register::HL),
            Operand::Register(Register::DE),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::ADD16,
    },
    // LD A,(DE) (1), - - - -
    InstructionDef {
        mnemonic: "LD A,(DE)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::DE),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // DEC DE (1), - - - -
    InstructionDef {
        mnemonic: "DEC DE",
        operands: [Operand::Register(Register::DE), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::DEC16,
    },
    // INC E (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC E (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD E,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD E,d8",
        operands: [Operand::Register(Register::E), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // RRA (1), 0 0 0 C
    InstructionDef {
        mnemonic: "RRA",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::RRA,
    },
    // JR NZ,r8 (2), - - - -
    InstructionDef {
        mnemonic: "JR NZ,r8",
        operands: [Operand::Relative8, Operand::None],
        len: 2,
        cycles: [12, 8],
        instr_type: InstructionType::JR_NZ,
    },
    // LD HL,d16 (3), - - - -
    InstructionDef {
        mnemonic: "LD HL,d16",
        operands: [Operand::Register(Register::HL), Operand::Immediate16],
        len: 3,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // LD (HL+),A (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL+),A",
        operands: [
            Operand::RegisterIndirectInc(Register::HL),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // INC HL (1), - - - -
    InstructionDef {
        mnemonic: "INC HL",
        operands: [Operand::Register(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::INC16,
    },
    // INC H (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC H (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD H,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD H,d8",
        operands: [Operand::Register(Register::H), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // DAA (1), Z - 0 C
    InstructionDef {
        mnemonic: "DAA",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DAA,
    },
    // JR Z,r8 (2), - - - -
    InstructionDef {
        mnemonic: "JR Z,r8",
        operands: [Operand::Relative8, Operand::None],
        len: 2,
        cycles: [12, 8],
        instr_type: InstructionType::JRZ,
    },
    // ADD HL,HL (1), - 0 H C
    InstructionDef {
        mnemonic: "ADD HL,HL",
        operands: [
            Operand::Register(Register::HL),
            Operand::Register(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::ADD16,
    },
    // LD A,(HL+) (1), - - - -
    InstructionDef {
        mnemonic: "LD A,(HL+)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirectInc(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // DEC HL (1), - - - -
    InstructionDef {
        mnemonic: "DEC HL",
        operands: [Operand::Register(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::DEC16,
    },
    // INC L (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC L (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD L,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD L,d8",
        operands: [Operand::Register(Register::L), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // CPL (1), - 1 1 -
    InstructionDef {
        mnemonic: "CPL",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CPL,
    },
    // JR NC,r8 (2), - - - -
    InstructionDef {
        mnemonic: "JR NC,r8",
        operands: [Operand::Relative8, Operand::None],
        len: 2,
        cycles: [12, 8],
        instr_type: InstructionType::JRNC,
    },
    // LD SP,d16 (3), - - - -
    InstructionDef {
        mnemonic: "LD SP,d16",
        operands: [Operand::Register(Register::SP), Operand::Immediate16],
        len: 3,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // LD (HL-),A (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL-),A",
        operands: [
            Operand::RegisterIndirectDec(Register::HL),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // INC SP (1), - - - -
    InstructionDef {
        mnemonic: "INC SP",
        operands: [Operand::Register(Register::SP), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::INC16,
    },
    // INC (HL) (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [12, 12],
        instr_type: InstructionType::INC8,
    },
    // DEC (HL) (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [12, 12],
        instr_type: InstructionType::DEC8,
    },
    // LD (HL),d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD (HL),d8",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::Immediate8],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // SCF (1), - 0 0 1
    InstructionDef {
        mnemonic: "SCF",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SCF,
    },
    // JR C,r8 (2), - - - -
    InstructionDef {
        mnemonic: "JR C,r8",
        operands: [Operand::Relative8, Operand::None],
        len: 2,
        cycles: [12, 8],
        instr_type: InstructionType::JRC,
    },
    // ADD HL,SP (1), - 0 H C
    InstructionDef {
        mnemonic: "ADD HL,SP",
        operands: [
            Operand::Register(Register::HL),
            Operand::Register(Register::SP),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::ADD16,
    },
    // LD A,(HL-) (1), - - - -
    InstructionDef {
        mnemonic: "LD A,(HL-)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirectDec(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // DEC SP (1), - - - -
    InstructionDef {
        mnemonic: "DEC SP",
        operands: [Operand::Register(Register::SP), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::DEC16,
    },
    // INC A (1), Z 0 H -
    InstructionDef {
        mnemonic: "INC A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INC8,
    },
    // DEC A (1), Z 1 H -
    InstructionDef {
        mnemonic: "DEC A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DEC8,
    },
    // LD A,d8 (2), - - - -
    InstructionDef {
        mnemonic: "LD A,d8",
        operands: [Operand::Register(Register::A), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // CCF (1), - 0 0 C
    InstructionDef {
        mnemonic: "CCF",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CCF,
    },
    // LD B,B (1), - - - -
    InstructionDef {
        mnemonic: "LD B,B",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD B,C (1), - - - -
    InstructionDef {
        mnemonic: "LD B,C",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD B,D (1), - - - -
    InstructionDef {
        mnemonic: "LD B,D",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD B,E (1), - - - -
    InstructionDef {
        mnemonic: "LD B,E",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD B,H (1), - - - -
    InstructionDef {
        mnemonic: "LD B,H",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD B,L (1), - - - -
    InstructionDef {
        mnemonic: "LD B,L",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD B,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD B,(HL)",
        operands: [
            Operand::Register(Register::B),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD B,A (1), - - - -
    InstructionDef {
        mnemonic: "LD B,A",
        operands: [
            Operand::Register(Register::B),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,B (1), - - - -
    InstructionDef {
        mnemonic: "LD C,B",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,C (1), - - - -
    InstructionDef {
        mnemonic: "LD C,C",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,D (1), - - - -
    InstructionDef {
        mnemonic: "LD C,D",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,E (1), - - - -
    InstructionDef {
        mnemonic: "LD C,E",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,H (1), - - - -
    InstructionDef {
        mnemonic: "LD C,H",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,L (1), - - - -
    InstructionDef {
        mnemonic: "LD C,L",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD C,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD C,(HL)",
        operands: [
            Operand::Register(Register::C),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD C,A (1), - - - -
    InstructionDef {
        mnemonic: "LD C,A",
        operands: [
            Operand::Register(Register::C),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,B (1), - - - -
    InstructionDef {
        mnemonic: "LD D,B",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,C (1), - - - -
    InstructionDef {
        mnemonic: "LD D,C",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,D (1), - - - -
    InstructionDef {
        mnemonic: "LD D,D",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,E (1), - - - -
    InstructionDef {
        mnemonic: "LD D,E",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,H (1), - - - -
    InstructionDef {
        mnemonic: "LD D,H",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,L (1), - - - -
    InstructionDef {
        mnemonic: "LD D,L",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD D,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD D,(HL)",
        operands: [
            Operand::Register(Register::D),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD D,A (1), - - - -
    InstructionDef {
        mnemonic: "LD D,A",
        operands: [
            Operand::Register(Register::D),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,B (1), - - - -
    InstructionDef {
        mnemonic: "LD E,B",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,C (1), - - - -
    InstructionDef {
        mnemonic: "LD E,C",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,D (1), - - - -
    InstructionDef {
        mnemonic: "LD E,D",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,E (1), - - - -
    InstructionDef {
        mnemonic: "LD E,E",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,H (1), - - - -
    InstructionDef {
        mnemonic: "LD E,H",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,L (1), - - - -
    InstructionDef {
        mnemonic: "LD E,L",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD E,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD E,(HL)",
        operands: [
            Operand::Register(Register::E),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD E,A (1), - - - -
    InstructionDef {
        mnemonic: "LD E,A",
        operands: [
            Operand::Register(Register::E),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,B (1), - - - -
    InstructionDef {
        mnemonic: "LD H,B",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,C (1), - - - -
    InstructionDef {
        mnemonic: "LD H,C",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,D (1), - - - -
    InstructionDef {
        mnemonic: "LD H,D",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,E (1), - - - -
    InstructionDef {
        mnemonic: "LD H,E",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,H (1), - - - -
    InstructionDef {
        mnemonic: "LD H,H",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,L (1), - - - -
    InstructionDef {
        mnemonic: "LD H,L",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD H,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD H,(HL)",
        operands: [
            Operand::Register(Register::H),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD H,A (1), - - - -
    InstructionDef {
        mnemonic: "LD H,A",
        operands: [
            Operand::Register(Register::H),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,B (1), - - - -
    InstructionDef {
        mnemonic: "LD L,B",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,C (1), - - - -
    InstructionDef {
        mnemonic: "LD L,C",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,D (1), - - - -
    InstructionDef {
        mnemonic: "LD L,D",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,E (1), - - - -
    InstructionDef {
        mnemonic: "LD L,E",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,H (1), - - - -
    InstructionDef {
        mnemonic: "LD L,H",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,L (1), - - - -
    InstructionDef {
        mnemonic: "LD L,L",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD L,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD L,(HL)",
        operands: [
            Operand::Register(Register::L),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD L,A (1), - - - -
    InstructionDef {
        mnemonic: "LD L,A",
        operands: [
            Operand::Register(Register::L),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD (HL),B (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),B",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD (HL),C (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),C",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD (HL),D (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),D",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD (HL),E (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),E",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD (HL),H (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),H",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD (HL),L (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),L",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // HALT (1), - - - -
    InstructionDef {
        mnemonic: "HALT",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::HALT,
    },
    // LD (HL),A (1), - - - -
    InstructionDef {
        mnemonic: "LD (HL),A",
        operands: [
            Operand::RegisterIndirect(Register::HL),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD A,B (1), - - - -
    InstructionDef {
        mnemonic: "LD A,B",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD A,C (1), - - - -
    InstructionDef {
        mnemonic: "LD A,C",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD A,D (1), - - - -
    InstructionDef {
        mnemonic: "LD A,D",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD A,E (1), - - - -
    InstructionDef {
        mnemonic: "LD A,E",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD A,H (1), - - - -
    InstructionDef {
        mnemonic: "LD A,H",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD A,L (1), - - - -
    InstructionDef {
        mnemonic: "LD A,L",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // LD A,(HL) (1), - - - -
    InstructionDef {
        mnemonic: "LD A,(HL)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD A,A (1), - - - -
    InstructionDef {
        mnemonic: "LD A,A",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::LD,
    },
    // ADD A,B (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,B",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADD A,C (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,C",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADD A,D (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,D",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADD A,E (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,E",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADD A,H (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,H",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADD A,L (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,L",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADD A,(HL) (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,(HL)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::ADD,
    },
    // ADD A,A (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,A",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADD,
    },
    // ADC A,B (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,B",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // ADC A,C (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,C",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // ADC A,D (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,D",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // ADC A,E (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,E",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // ADC A,H (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,H",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // ADC A,L (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,L",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // ADC A,(HL) (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,(HL)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::ADC,
    },
    // ADC A,A (1), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,A",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::ADC,
    },
    // SUB B (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SUB C (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SUB D (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SUB E (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SUB H (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SUB L (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SUB (HL) (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::SUB,
    },
    // SUB A (1), Z 1 H C
    InstructionDef {
        mnemonic: "SUB A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SUB,
    },
    // SBC A,B (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,B",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::B),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // SBC A,C (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,C",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::C),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // SBC A,D (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,D",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::D),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // SBC A,E (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,E",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::E),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // SBC A,H (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,H",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::H),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // SBC A,L (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,L",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::L),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // SBC A,(HL) (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,(HL)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::SBC,
    },
    // SBC A,A (1), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,A",
        operands: [
            Operand::Register(Register::A),
            Operand::Register(Register::A),
        ],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::SBC,
    },
    // AND B (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // AND C (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // AND D (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // AND E (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // AND H (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // AND L (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // AND (HL) (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::AND,
    },
    // AND A (1), Z 0 1 0
    InstructionDef {
        mnemonic: "AND A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::AND,
    },
    // XOR B (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // XOR C (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // XOR D (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // XOR E (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // XOR H (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // XOR L (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // XOR (HL) (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::XOR,
    },
    // XOR A (1), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::XOR,
    },
    // OR B (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // OR C (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // OR D (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // OR E (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // OR H (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // OR L (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // OR (HL) (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::OR,
    },
    // OR A (1), Z 0 0 0
    InstructionDef {
        mnemonic: "OR A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::OR,
    },
    // CP B (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // CP C (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // CP D (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // CP E (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // CP H (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // CP L (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // CP (HL) (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::CP,
    },
    // CP A (1), Z 1 H C
    InstructionDef {
        mnemonic: "CP A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::CP,
    },
    // RET NZ (1), - - - -
    InstructionDef {
        mnemonic: "RET NZ",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [20, 8],
        instr_type: InstructionType::RET_NZ,
    },
    // POP BC (1), - - - -
    InstructionDef {
        mnemonic: "POP BC",
        operands: [Operand::Register(Register::BC), Operand::None],
        len: 1,
        cycles: [12, 12],
        instr_type: InstructionType::POP,
    },
    // JP NZ,a16 (3), - - - -
    InstructionDef {
        mnemonic: "JP NZ,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [16, 12],
        instr_type: InstructionType::JP_NZ,
    },
    // JP a16 (3), - - - -
    InstructionDef {
        mnemonic: "JP a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [16, 16],
        instr_type: InstructionType::JP,
    },
    // CALL NZ,a16 (3), - - - -
    InstructionDef {
        mnemonic: "CALL NZ,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [24, 12],
        instr_type: InstructionType::CALL_NZ,
    },
    // PUSH BC (1), - - - -
    InstructionDef {
        mnemonic: "PUSH BC",
        operands: [Operand::Register(Register::BC), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::PUSH,
    },
    // ADD A,d8 (2), Z 0 H C
    InstructionDef {
        mnemonic: "ADD A,d8",
        operands: [Operand::Register(Register::A), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::ADD,
    },
    // RST 00H (1), - - - -
    InstructionDef {
        mnemonic: "RST 00H",
        operands: [Operand::Constant(0x00), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // RET Z (1), - - - -
    InstructionDef {
        mnemonic: "RET Z",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [20, 8],
        instr_type: InstructionType::RET_Z,
    },
    // RET (1), - - - -
    InstructionDef {
        mnemonic: "RET",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RET,
    },
    // JP Z,a16 (3), - - - -
    InstructionDef {
        mnemonic: "JP Z,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [16, 12],
        instr_type: InstructionType::JP_Z,
    },
    // PREFIX CB (1), - - - -
    InstructionDef {
        mnemonic: "PREFIX CB",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::INVALID,
    },
    // CALL Z,a16 (3), - - - -
    InstructionDef {
        mnemonic: "CALL Z,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [24, 12],
        instr_type: InstructionType::CALL_Z,
    },
    // CALL a16 (3), - - - -
    InstructionDef {
        mnemonic: "CALL a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [24, 24],
        instr_type: InstructionType::CALL,
    },
    // ADC A,d8 (2), Z 0 H C
    InstructionDef {
        mnemonic: "ADC A,d8",
        operands: [Operand::Register(Register::A), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::ADC,
    },
    // RST 08H (1), - - - -
    InstructionDef {
        mnemonic: "RST 08H",
        operands: [Operand::Constant(0x08), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // RET NC (1), - - - -
    InstructionDef {
        mnemonic: "RET NC",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [20, 8],
        instr_type: InstructionType::RET_NC,
    },
    // POP DE (1), - - - -
    InstructionDef {
        mnemonic: "POP DE",
        operands: [Operand::Register(Register::DE), Operand::None],
        len: 1,
        cycles: [12, 12],
        instr_type: InstructionType::POP,
    },
    // JP NC,a16 (3), - - - -
    InstructionDef {
        mnemonic: "JP NC,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [16, 12],
        instr_type: InstructionType::JP_NC,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // CALL NC,a16 (3), - - - -
    InstructionDef {
        mnemonic: "CALL NC,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [24, 12],
        instr_type: InstructionType::CALL_NC,
    },
    // PUSH DE (1), - - - -
    InstructionDef {
        mnemonic: "PUSH DE",
        operands: [Operand::Register(Register::DE), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::PUSH,
    },
    // SUB d8 (2), Z 1 H C
    InstructionDef {
        mnemonic: "SUB d8",
        operands: [Operand::Immediate8, Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SUB,
    },
    // RST 10H (1), - - - -
    InstructionDef {
        mnemonic: "RST 10H",
        operands: [Operand::Constant(0x10), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // RET C (1), - - - -
    InstructionDef {
        mnemonic: "RET C",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [20, 8],
        instr_type: InstructionType::RET_C,
    },
    // RETI (1), - - - -
    InstructionDef {
        mnemonic: "RETI",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RETI,
    },
    // JP C,a16 (3), - - - -
    InstructionDef {
        mnemonic: "JP C,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [16, 12],
        instr_type: InstructionType::JP_C,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // CALL C,a16 (3), - - - -
    InstructionDef {
        mnemonic: "CALL C,a16",
        operands: [Operand::ImmediateIndirect16, Operand::None],
        len: 3,
        cycles: [24, 12],
        instr_type: InstructionType::CALL_C,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // SBC A,d8 (2), Z 1 H C
    InstructionDef {
        mnemonic: "SBC A,d8",
        operands: [Operand::Register(Register::A), Operand::Immediate8],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SBC,
    },
    // RST 18H (1), - - - -
    InstructionDef {
        mnemonic: "RST 18H",
        operands: [Operand::Constant(0x18), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // LDH (a8),A (2), - - - -
    InstructionDef {
        mnemonic: "LDH (a8),A",
        operands: [Operand::ImmediateIndirect8, Operand::Register(Register::A)],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // POP HL (1), - - - -
    InstructionDef {
        mnemonic: "POP HL",
        operands: [Operand::Register(Register::HL), Operand::None],
        len: 1,
        cycles: [12, 12],
        instr_type: InstructionType::POP,
    },
    // LD (C),A (2), - - - -
    InstructionDef {
        mnemonic: "LD (C),A",
        operands: [
            Operand::RegisterIndirect(Register::C),
            Operand::Register(Register::A),
        ],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // PUSH HL (1), - - - -
    InstructionDef {
        mnemonic: "PUSH HL",
        operands: [Operand::Register(Register::HL), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::PUSH,
    },
    // AND d8 (2), Z 0 1 0
    InstructionDef {
        mnemonic: "AND d8",
        operands: [Operand::Immediate8, Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::AND,
    },
    // RST 20H (1), - - - -
    InstructionDef {
        mnemonic: "RST 20H",
        operands: [Operand::Constant(0x20), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // ADD SP,r8 (2), 0 0 H C
    InstructionDef {
        mnemonic: "ADD SP,r8",
        operands: [Operand::Register(Register::SP), Operand::Relative8],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::ADD_SP,
    },
    // JP (HL) (1), - - - -
    InstructionDef {
        mnemonic: "JP (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::JP,
    },
    // LD (a16),A (3), - - - -
    InstructionDef {
        mnemonic: "LD (a16),A",
        operands: [Operand::ImmediateIndirect16, Operand::Register(Register::A)],
        len: 3,
        cycles: [16, 16],
        instr_type: InstructionType::LD,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // XOR d8 (2), Z 0 0 0
    InstructionDef {
        mnemonic: "XOR d8",
        operands: [Operand::Immediate8, Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::XOR,
    },
    // RST 28H (1), - - - -
    InstructionDef {
        mnemonic: "RST 28H",
        operands: [Operand::Constant(0x28), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // LDH A,(a8) (2), - - - -
    InstructionDef {
        mnemonic: "LDH A,(a8)",
        operands: [Operand::Register(Register::A), Operand::ImmediateIndirect8],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::LD,
    },
    // POP AF (1), Z N H C
    InstructionDef {
        mnemonic: "POP AF",
        operands: [Operand::Register(Register::AF), Operand::None],
        len: 1,
        cycles: [12, 12],
        instr_type: InstructionType::POP,
    },
    // LD A,(C) (2), - - - -
    InstructionDef {
        mnemonic: "LD A,(C)",
        operands: [
            Operand::Register(Register::A),
            Operand::RegisterIndirect(Register::C),
        ],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // DI (1), - - - -
    InstructionDef {
        mnemonic: "DI",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::DI,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // PUSH AF (1), - - - -
    InstructionDef {
        mnemonic: "PUSH AF",
        operands: [Operand::Register(Register::AF), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::PUSH,
    },
    // OR d8 (2), Z 0 0 0
    InstructionDef {
        mnemonic: "OR d8",
        operands: [Operand::Immediate8, Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::OR,
    },
    // RST 30H (1), - - - -
    InstructionDef {
        mnemonic: "RST 30H",
        operands: [Operand::Constant(0x30), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
    // LD HL,SP+r8 (2), 0 0 H C
    InstructionDef {
        mnemonic: "LD HL,SP+r8",
        operands: [Operand::Register(Register::HL), Operand::SPRelative8],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::ADD_SP,
    },
    // LD SP,HL (1), - - - -
    InstructionDef {
        mnemonic: "LD SP,HL",
        operands: [
            Operand::Register(Register::SP),
            Operand::Register(Register::HL),
        ],
        len: 1,
        cycles: [8, 8],
        instr_type: InstructionType::LD,
    },
    // LD A,(a16) (3), - - - -
    InstructionDef {
        mnemonic: "LD A,(a16)",
        operands: [Operand::Register(Register::A), Operand::ImmediateIndirect16],
        len: 3,
        cycles: [16, 16],
        instr_type: InstructionType::LD,
    },
    // EI (1), - - - -
    InstructionDef {
        mnemonic: "EI",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [4, 4],
        instr_type: InstructionType::EI,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // INVALID
    InstructionDef {
        mnemonic: "INVALID",
        operands: [Operand::None, Operand::None],
        len: 1,
        cycles: [0, 0],
        instr_type: InstructionType::INVALID,
    },
    // CP d8 (2), Z 1 H C
    InstructionDef {
        mnemonic: "CP d8",
        operands: [Operand::Immediate8, Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::CP,
    },
    // RST 38H (1), - - - -
    InstructionDef {
        mnemonic: "RST 38H",
        operands: [Operand::Constant(0x38), Operand::None],
        len: 1,
        cycles: [16, 16],
        instr_type: InstructionType::RST,
    },
];

/// 0xCB prefix instruction table, parsed from https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
pub const INSTRUCTIONS_CB: [InstructionDef; 256] = [
    // RLC B (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RLC C (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RLC D (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RLC E (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RLC H (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RLC L (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RLC (HL) (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RLC,
    },
    // RLC A (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RLC A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RLC,
    },
    // RRC B (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RRC C (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RRC D (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RRC E (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RRC H (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RRC L (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RRC (HL) (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RRC,
    },
    // RRC A (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RRC A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RRC,
    },
    // RL B (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RL C (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RL D (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RL E (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RL H (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RL L (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RL (HL) (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RL,
    },
    // RL A (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RL A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RL,
    },
    // RR B (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // RR C (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // RR D (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // RR E (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // RR H (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // RR L (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // RR (HL) (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RR,
    },
    // RR A (2), Z 0 0 C
    InstructionDef {
        mnemonic: "RR A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RR,
    },
    // SLA B (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SLA C (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SLA D (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SLA E (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SLA H (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SLA L (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SLA (HL) (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SLA,
    },
    // SLA A (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SLA A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SLA,
    },
    // SRA B (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SRA C (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SRA D (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SRA E (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SRA H (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SRA L (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SRA (HL) (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SRA,
    },
    // SRA A (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SRA A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRA,
    },
    // SWAP B (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SWAP C (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SWAP D (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SWAP E (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SWAP H (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SWAP L (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SWAP (HL) (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SWAP,
    },
    // SWAP A (2), Z 0 0 0
    InstructionDef {
        mnemonic: "SWAP A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SWAP,
    },
    // SRL B (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL B",
        operands: [Operand::Register(Register::B), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // SRL C (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL C",
        operands: [Operand::Register(Register::C), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // SRL D (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL D",
        operands: [Operand::Register(Register::D), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // SRL E (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL E",
        operands: [Operand::Register(Register::E), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // SRL H (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL H",
        operands: [Operand::Register(Register::H), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // SRL L (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL L",
        operands: [Operand::Register(Register::L), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // SRL (HL) (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL (HL)",
        operands: [Operand::RegisterIndirect(Register::HL), Operand::None],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SRL,
    },
    // SRL A (2), Z 0 0 C
    InstructionDef {
        mnemonic: "SRL A",
        operands: [Operand::Register(Register::A), Operand::None],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SRL,
    },
    // BIT 0,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,B",
        operands: [Operand::Constant(0), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,C",
        operands: [Operand::Constant(0), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,D",
        operands: [Operand::Constant(0), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,E",
        operands: [Operand::Constant(0), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,H",
        operands: [Operand::Constant(0), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,L",
        operands: [Operand::Constant(0), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,(HL)",
        operands: [
            Operand::Constant(0),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 0,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 0,A",
        operands: [Operand::Constant(0), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,B",
        operands: [Operand::Constant(1), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,C",
        operands: [Operand::Constant(1), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,D",
        operands: [Operand::Constant(1), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,E",
        operands: [Operand::Constant(1), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,H",
        operands: [Operand::Constant(1), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,L",
        operands: [Operand::Constant(1), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,(HL)",
        operands: [
            Operand::Constant(1),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 1,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 1,A",
        operands: [Operand::Constant(1), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,B",
        operands: [Operand::Constant(2), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,C",
        operands: [Operand::Constant(2), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,D",
        operands: [Operand::Constant(2), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,E",
        operands: [Operand::Constant(2), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,H",
        operands: [Operand::Constant(2), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,L",
        operands: [Operand::Constant(2), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,(HL)",
        operands: [
            Operand::Constant(2),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 2,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 2,A",
        operands: [Operand::Constant(2), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,B",
        operands: [Operand::Constant(3), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,C",
        operands: [Operand::Constant(3), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,D",
        operands: [Operand::Constant(3), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,E",
        operands: [Operand::Constant(3), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,H",
        operands: [Operand::Constant(3), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,L",
        operands: [Operand::Constant(3), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,(HL)",
        operands: [
            Operand::Constant(3),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 3,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 3,A",
        operands: [Operand::Constant(3), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,B",
        operands: [Operand::Constant(4), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,C",
        operands: [Operand::Constant(4), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,D",
        operands: [Operand::Constant(4), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,E",
        operands: [Operand::Constant(4), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,H",
        operands: [Operand::Constant(4), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,L",
        operands: [Operand::Constant(4), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,(HL)",
        operands: [
            Operand::Constant(4),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 4,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 4,A",
        operands: [Operand::Constant(4), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,B",
        operands: [Operand::Constant(5), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,C",
        operands: [Operand::Constant(5), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,D",
        operands: [Operand::Constant(5), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,E",
        operands: [Operand::Constant(5), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,H",
        operands: [Operand::Constant(5), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,L",
        operands: [Operand::Constant(5), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,(HL)",
        operands: [
            Operand::Constant(5),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 5,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 5,A",
        operands: [Operand::Constant(5), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,B",
        operands: [Operand::Constant(6), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,C",
        operands: [Operand::Constant(6), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,D",
        operands: [Operand::Constant(6), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,E",
        operands: [Operand::Constant(6), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,H",
        operands: [Operand::Constant(6), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,L",
        operands: [Operand::Constant(6), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,(HL)",
        operands: [
            Operand::Constant(6),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 6,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 6,A",
        operands: [Operand::Constant(6), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,B (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,B",
        operands: [Operand::Constant(7), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,C (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,C",
        operands: [Operand::Constant(7), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,D (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,D",
        operands: [Operand::Constant(7), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,E (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,E",
        operands: [Operand::Constant(7), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,H (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,H",
        operands: [Operand::Constant(7), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,L (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,L",
        operands: [Operand::Constant(7), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,(HL) (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,(HL)",
        operands: [
            Operand::Constant(7),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [12, 12],
        instr_type: InstructionType::BIT,
    },
    // BIT 7,A (2), Z 0 1 -
    InstructionDef {
        mnemonic: "BIT 7,A",
        operands: [Operand::Constant(7), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::BIT,
    },
    // RES 0,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,B",
        operands: [Operand::Constant(0), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 0,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,C",
        operands: [Operand::Constant(0), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 0,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,D",
        operands: [Operand::Constant(0), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 0,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,E",
        operands: [Operand::Constant(0), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 0,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,H",
        operands: [Operand::Constant(0), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 0,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,L",
        operands: [Operand::Constant(0), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 0,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,(HL)",
        operands: [
            Operand::Constant(0),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 0,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 0,A",
        operands: [Operand::Constant(0), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,B",
        operands: [Operand::Constant(1), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,C",
        operands: [Operand::Constant(1), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,D",
        operands: [Operand::Constant(1), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,E",
        operands: [Operand::Constant(1), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,H",
        operands: [Operand::Constant(1), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,L",
        operands: [Operand::Constant(1), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 1,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,(HL)",
        operands: [
            Operand::Constant(1),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 1,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 1,A",
        operands: [Operand::Constant(1), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,B",
        operands: [Operand::Constant(2), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,C",
        operands: [Operand::Constant(2), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,D",
        operands: [Operand::Constant(2), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,E",
        operands: [Operand::Constant(2), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,H",
        operands: [Operand::Constant(2), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,L",
        operands: [Operand::Constant(2), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 2,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,(HL)",
        operands: [
            Operand::Constant(2),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 2,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 2,A",
        operands: [Operand::Constant(2), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,B",
        operands: [Operand::Constant(3), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,C",
        operands: [Operand::Constant(3), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,D",
        operands: [Operand::Constant(3), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,E",
        operands: [Operand::Constant(3), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,H",
        operands: [Operand::Constant(3), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,L",
        operands: [Operand::Constant(3), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 3,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,(HL)",
        operands: [
            Operand::Constant(3),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 3,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 3,A",
        operands: [Operand::Constant(3), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,B",
        operands: [Operand::Constant(4), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,C",
        operands: [Operand::Constant(4), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,D",
        operands: [Operand::Constant(4), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,E",
        operands: [Operand::Constant(4), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,H",
        operands: [Operand::Constant(4), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,L",
        operands: [Operand::Constant(4), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 4,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,(HL)",
        operands: [
            Operand::Constant(4),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 4,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 4,A",
        operands: [Operand::Constant(4), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,B",
        operands: [Operand::Constant(5), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,C",
        operands: [Operand::Constant(5), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,D",
        operands: [Operand::Constant(5), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,E",
        operands: [Operand::Constant(5), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,H",
        operands: [Operand::Constant(5), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,L",
        operands: [Operand::Constant(5), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 5,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,(HL)",
        operands: [
            Operand::Constant(5),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 5,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 5,A",
        operands: [Operand::Constant(5), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,B",
        operands: [Operand::Constant(6), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,C",
        operands: [Operand::Constant(6), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,D",
        operands: [Operand::Constant(6), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,E",
        operands: [Operand::Constant(6), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,H",
        operands: [Operand::Constant(6), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,L",
        operands: [Operand::Constant(6), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 6,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,(HL)",
        operands: [
            Operand::Constant(6),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 6,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 6,A",
        operands: [Operand::Constant(6), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,B (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,B",
        operands: [Operand::Constant(7), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,C (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,C",
        operands: [Operand::Constant(7), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,D (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,D",
        operands: [Operand::Constant(7), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,E (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,E",
        operands: [Operand::Constant(7), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,H (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,H",
        operands: [Operand::Constant(7), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,L (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,L",
        operands: [Operand::Constant(7), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // RES 7,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,(HL)",
        operands: [
            Operand::Constant(7),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::RES,
    },
    // RES 7,A (2), - - - -
    InstructionDef {
        mnemonic: "RES 7,A",
        operands: [Operand::Constant(7), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::RES,
    },
    // SET 0,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,B",
        operands: [Operand::Constant(0), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 0,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,C",
        operands: [Operand::Constant(0), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 0,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,D",
        operands: [Operand::Constant(0), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 0,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,E",
        operands: [Operand::Constant(0), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 0,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,H",
        operands: [Operand::Constant(0), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 0,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,L",
        operands: [Operand::Constant(0), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 0,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,(HL)",
        operands: [
            Operand::Constant(0),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 0,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 0,A",
        operands: [Operand::Constant(0), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,B",
        operands: [Operand::Constant(1), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,C",
        operands: [Operand::Constant(1), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,D",
        operands: [Operand::Constant(1), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,E",
        operands: [Operand::Constant(1), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,H",
        operands: [Operand::Constant(1), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,L",
        operands: [Operand::Constant(1), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 1,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,(HL)",
        operands: [
            Operand::Constant(1),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 1,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 1,A",
        operands: [Operand::Constant(1), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,B",
        operands: [Operand::Constant(2), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,C",
        operands: [Operand::Constant(2), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,D",
        operands: [Operand::Constant(2), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,E",
        operands: [Operand::Constant(2), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,H",
        operands: [Operand::Constant(2), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,L",
        operands: [Operand::Constant(2), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 2,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,(HL)",
        operands: [
            Operand::Constant(2),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 2,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 2,A",
        operands: [Operand::Constant(2), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,B",
        operands: [Operand::Constant(3), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,C",
        operands: [Operand::Constant(3), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,D",
        operands: [Operand::Constant(3), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,E",
        operands: [Operand::Constant(3), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,H",
        operands: [Operand::Constant(3), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,L",
        operands: [Operand::Constant(3), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 3,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,(HL)",
        operands: [
            Operand::Constant(3),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 3,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 3,A",
        operands: [Operand::Constant(3), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,B",
        operands: [Operand::Constant(4), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,C",
        operands: [Operand::Constant(4), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,D",
        operands: [Operand::Constant(4), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,E",
        operands: [Operand::Constant(4), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,H",
        operands: [Operand::Constant(4), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,L",
        operands: [Operand::Constant(4), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 4,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,(HL)",
        operands: [
            Operand::Constant(4),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 4,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 4,A",
        operands: [Operand::Constant(4), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,B",
        operands: [Operand::Constant(5), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,C",
        operands: [Operand::Constant(5), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,D",
        operands: [Operand::Constant(5), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,E",
        operands: [Operand::Constant(5), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,H",
        operands: [Operand::Constant(5), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,L",
        operands: [Operand::Constant(5), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 5,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,(HL)",
        operands: [
            Operand::Constant(5),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 5,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 5,A",
        operands: [Operand::Constant(5), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,B",
        operands: [Operand::Constant(6), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,C",
        operands: [Operand::Constant(6), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,D",
        operands: [Operand::Constant(6), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,E",
        operands: [Operand::Constant(6), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,H",
        operands: [Operand::Constant(6), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,L",
        operands: [Operand::Constant(6), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 6,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,(HL)",
        operands: [
            Operand::Constant(6),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 6,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 6,A",
        operands: [Operand::Constant(6), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,B (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,B",
        operands: [Operand::Constant(7), Operand::Register(Register::B)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,C (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,C",
        operands: [Operand::Constant(7), Operand::Register(Register::C)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,D (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,D",
        operands: [Operand::Constant(7), Operand::Register(Register::D)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,E (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,E",
        operands: [Operand::Constant(7), Operand::Register(Register::E)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,H (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,H",
        operands: [Operand::Constant(7), Operand::Register(Register::H)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,L (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,L",
        operands: [Operand::Constant(7), Operand::Register(Register::L)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
    // SET 7,(HL) (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,(HL)",
        operands: [
            Operand::Constant(7),
            Operand::RegisterIndirect(Register::HL),
        ],
        len: 2,
        cycles: [16, 16],
        instr_type: InstructionType::SET,
    },
    // SET 7,A (2), - - - -
    InstructionDef {
        mnemonic: "SET 7,A",
        operands: [Operand::Constant(7), Operand::Register(Register::A)],
        len: 2,
        cycles: [8, 8],
        instr_type: InstructionType::SET,
    },
];
