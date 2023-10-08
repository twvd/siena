/// Instruction addressing mode
pub enum AddressingMode {
    /// Absolute
    /// 3 bytes, $OP $LL $HH
    Absolute,
    /// Absolute,X
    /// 3 bytes, $OP $LL $HH
    AbsoluteX,
    /// Absolute,Y
    /// 3 bytes, $OP $LL $HH
    AbsoluteY,
    /// (Absolute)
    /// 3 bytes, $OP $LL $HH
    AbsolutePtr16,
    /// [Absolute]
    /// 3 bytes, $OP $LL $HH
    AbsolutePtr24,
    /// (Absolute,X)
    /// 3 bytes, $OP $LL $HH
    AbsoluteXPtr16,
    /// Accumulator
    /// 1 byte, $OP
    Accumulator,
    /// DIRECT
    /// 2 bytes, $OP $LL
    Direct,
    /// DIRECT,X
    /// 2 bytes, $OP $LL
    DirectX,
    /// DIRECT,Y
    /// 2 bytes, $OP $LL
    DirectY,
    /// (DIRECT)
    /// 2 bytes, $OP $LL
    DirectPtr16,
    /// [DIRECT]
    /// 2 bytes, $OP $LL
    DirectPtr24,
    /// (DIRECT,X)
    /// 2 bytes, $OP $LL
    DirectXPtr16,
    /// (DIRECT),Y
    /// 2 bytes, $OP $LL
    DirectPtr16Y,
    /// [DIRECT],Y
    /// 2 bytes, $OP $LL
    DirectPtr24Y,
    /// Immediate (8-bit)
    /// Length: 2 bytes, $OP $LL (for 8-bit data)
    Immediate8,
    /// Immediate (16-bit)
    /// 3 bytes, $OP $LL $HH
    Immediate16,
    /// Immediate (depending on M-flag)
    /// 2-3 bytes, $OP $LL ($HH)
    ImmediateM,
    /// Immediate (depending on X-flag)
    /// 2-3 bytes, $OP $LL ($HH)
    ImmediateX,
    /// Implied
    /// 1 byte, $OP
    Implied,
    /// Long
    /// 4 bytes, $OP $LL $MM $HH
    Long,
    /// Long,X
    /// 4 bytes, $OP $LL $MM $HH
    LongX,
    /// Relative 8-bit
    /// 2 bytes, $OP $LL
    Relative8,
    /// Relative 16-bit
    /// 2 bytes, $OP $LL $HH
    Relative16,
    /// Source,Desination
    /// 3 bytes, $OP $TT $SS
    SrcDest,
    /// Stack,S
    /// 2 bytes, $OP $LL
    StackS,
    /// (Stack,S),Y
    /// 2 bytes, $OP $LL
    StackSPtr16Y,
}

/// Instruction types
pub enum InstructionType {
    MVN,
    MVP,
    TYA,
    TYX,
    CMP,
    DEC,
    SEP,
    XBA,
    SEI,
    SEC,
    JMP,
    SED,
    DEX,
    DEY,
    ORA,
    PHD,
    PHA,
    PHB,
    INX,
    INY,
    AND,
    PHK,
    SBC,
    INC,
    TAX,
    TAY,
    PHP,
    PHY,
    TRB,
    PHX,
    TSB,
    TSC,
    WDM,
    CLV,
    XCE,
    CLC,
    CLD,
    TSX,
    CLI,
    ADC,
    BMI,
    BEQ,
    BRK,
    BRL,
    BRA,
    JSR,
    ROL,
    COP,
    STA,
    STZ,
    STX,
    STY,
    LSR,
    STP,
    ROR,
    JSL,
    ASL,
    TCS,
    TCD,
    PLY,
    PLX,
    PLP,
    PLA,
    NOP,
    PLB,
    PLD,
    BCC,
    BPL,
    TDC,
    BCS,
    PEA,
    RTS,
    WAI,
    PEI,
    PER,
    RTI,
    RTL,
    LDY,
    LDX,
    LDA,
    BVC,
    BVS,
    BNE,
    CPX,
    CPY,
    REP,
    TXA,
    TXS,
    TXY,
    EOR,
    Undefined,
}

/// A definition in the instruction (op code) table
pub struct InstructionDef {
    /// String representation
    /// '@' represents an immediate value to be replacee
    pub mnemonic: &'static str,

    /// Addressing mode
    pub mode: AddressingMode,

    /// Length
    pub len: usize,

    /// Instruction type
    pub instr_type: InstructionType,
}
