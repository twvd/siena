use anyhow::Result;
use num_traits::ToPrimitive;

use crate::snes::bus::{Address, Bus, BusIterator, ADDRESS_MASK};
use crate::tickable::Ticks;

use super::alu;
use super::instruction::{AddressingMode, Instruction, InstructionType};
use super::regs::{Flag, Register, RegisterFile};

/// Main SNES CPU (65816)
pub struct Cpu65816<TBus: Bus> {
    pub bus: TBus,
    pub regs: RegisterFile,
    pub cycles: Ticks,
}

impl<TBus> Cpu65816<TBus>
where
    TBus: Bus,
{
    pub fn new(bus: TBus, reset_addr: u16) -> Self {
        let mut cpu = Self {
            bus,
            regs: RegisterFile::new(),
            cycles: 0,
        };
        cpu.regs.pc = reset_addr;
        cpu.regs.p = (1 << Flag::M.to_u8().unwrap()) | (1 << Flag::X.to_u8().unwrap());
        cpu.regs.emulation = true;

        cpu
    }

    pub fn dump_state(&self) -> String {
        format!(
            "{} - {}\n --> {}",
            self.cycles,
            self.regs,
            self.peek_next_instr().unwrap()
        )
    }

    /// Fetches and decodes the next instruction at PC
    pub fn peek_next_instr(&self) -> Result<Instruction> {
        let mut busiter = BusIterator::new_from(&self.bus, self.regs.get_full_pc());
        Instruction::decode(
            &mut busiter,
            self.regs.test_flag(Flag::M),
            self.regs.test_flag(Flag::X),
        )
    }

    /// Fetches and decodes the next instruction at PC
    pub fn fetch_next_instr(&mut self) -> Result<Instruction> {
        let mut fetched: Vec<u8> = vec![];

        for p in 0.. {
            let pc = (self.regs.k as Address) << 16 | self.regs.pc.wrapping_add(p) as Address;
            match Instruction::decode(
                &mut fetched.clone().into_iter(),
                self.regs.test_flag(Flag::M),
                self.regs.test_flag(Flag::X),
            ) {
                Err(_) => fetched.push(self.read_tick(pc)),
                Ok(i) => return Ok(i),
            }
        }

        unreachable!()
    }

    /// Executes one CPU step (one instruction).
    pub fn step(&mut self) -> Result<()> {
        let instr = self.fetch_next_instr()?;

        self.regs.pc = self.regs.pc.wrapping_add(instr.len as u16);
        let _cycles = self.execute_instruction(&instr)?;

        Ok(())
    }

    /// Tick peripherals
    fn tick_bus(&mut self, cycles: Ticks) -> Result<()> {
        if cycles == 0 {
            return Ok(());
        }

        self.cycles += cycles;
        self.bus.tick(cycles)
    }

    /// Reads a memory location while ticking peripherals
    /// for the access time.
    fn read_tick(&mut self, addr: Address) -> u8 {
        let v = self.bus.read(addr);
        self.tick_bus(1).unwrap();
        v
    }

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 16-bits.
    fn read16_tick_a16(&mut self, addr: Address) -> u16 {
        let mut v = self.bus.read(addr) as u16;
        self.tick_bus(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        v |= (self.bus.read(hi_addr) as u16) << 8;
        self.tick_bus(1).unwrap();
        v
    }

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 24-bits.
    fn read16_tick_a24(&mut self, addr: Address) -> u16 {
        let mut v = self.bus.read(addr) as u16;
        self.tick_bus(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        v |= (self.bus.read(hi_addr) as u16) << 8;
        self.tick_bus(1).unwrap();
        v
    }

    /// Reads 24-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 16-bits.
    fn read24_tick_a16(&mut self, addr: Address) -> u32 {
        let mut v = self.bus.read(addr) as u32;
        self.tick_bus(1).unwrap();
        let mid_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        v |= (self.bus.read(mid_addr) as u32) << 8;
        self.tick_bus(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(2));
        v |= (self.bus.read(hi_addr) as u32) << 16;
        self.tick_bus(1).unwrap();
        v
    }

    /// Writes a memory location while ticking peripherals
    /// for the access time.
    fn write_tick(&mut self, addr: Address, val: u8) {
        self.bus.write(addr, val);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 16-bit address wrap.
    fn write16_tick_a16(&mut self, addr: Address, val: u16) {
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 16-bit address wrap, descending temporal order.
    fn write16_tick_a16_desc(&mut self, addr: Address, val: u16) {
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 24-bit address wrap.
    fn write16_tick_a24(&mut self, addr: Address, val: u16) {
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// 24-bit address wrap, descending temporal order.
    fn write16_tick_a24_desc(&mut self, addr: Address, val: u16) {
        let hi_addr = addr.wrapping_add(1);
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// Selects wrap based on addressing mode.
    fn write16_tick_a(&mut self, instr: &Instruction, addr: Address, value: u16) {
        match instr.def.mode {
            AddressingMode::Direct | AddressingMode::DirectX | AddressingMode::StackS => {
                self.write16_tick_a16(addr, value)
            }
            _ => self.write16_tick_a24(addr, value),
        }
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// Selects wrap based on addressing mode, descending temporal order.
    fn write16_tick_a_desc(&mut self, instr: &Instruction, addr: Address, value: u16) {
        match instr.def.mode {
            AddressingMode::Direct | AddressingMode::DirectX | AddressingMode::StackS => {
                self.write16_tick_a16_desc(addr, value)
            }
            _ => self.write16_tick_a24_desc(addr, value),
        }
    }

    /// Executes an instruction.
    fn execute_instruction(&mut self, instr: &Instruction) -> Result<()> {
        match instr.def.instr_type {
            InstructionType::NOP => self.tick_bus(1),
            InstructionType::WDM => Ok(()),
            InstructionType::CLC => self.op_clx(Flag::C),
            InstructionType::CLD => self.op_clx(Flag::D),
            InstructionType::CLI => self.op_clx(Flag::I),
            InstructionType::CLV => self.op_clx(Flag::V),
            InstructionType::SEC => self.op_sex(Flag::C),
            InstructionType::SED => self.op_sex(Flag::D),
            InstructionType::SEI => self.op_sex(Flag::I),
            InstructionType::XBA => self.op_xba(),
            InstructionType::XCE => self.op_xce(),
            InstructionType::TCD => self.op_txx_16b(Register::C, Register::D, true),
            InstructionType::TCS => self.op_txx_16b(Register::C, Register::S, false),
            InstructionType::TDC => self.op_txx_16b(Register::D, Register::C, true),
            InstructionType::TSC => self.op_txx_16b(Register::S, Register::C, true),
            InstructionType::TAX => self.op_txx(Register::C, Register::X, Flag::X),
            InstructionType::TAY => self.op_txx(Register::C, Register::Y, Flag::X),
            InstructionType::TSX => self.op_txx(Register::S, Register::X, Flag::X),
            InstructionType::TXA => self.op_txx(Register::X, Register::C, Flag::M),
            InstructionType::TXS => self.op_txx_16b(Register::X, Register::S, false),
            InstructionType::TXY => self.op_txx(Register::X, Register::Y, Flag::X),
            InstructionType::TYA => self.op_txx(Register::Y, Register::C, Flag::M),
            InstructionType::TYX => self.op_txx(Register::Y, Register::X, Flag::X),
            InstructionType::STZ => self.op_stz(&instr),
            InstructionType::STA => self.op_stx_reg(&instr, Register::C, Flag::M),
            InstructionType::STX => self.op_stx_reg(&instr, Register::X, Flag::X),
            InstructionType::STY => self.op_stx_reg(&instr, Register::Y, Flag::X),
            InstructionType::LDA => self.op_load(&instr, Register::C, Flag::M),
            InstructionType::LDX => self.op_load(&instr, Register::X, Flag::X),
            InstructionType::LDY => self.op_load(&instr, Register::Y, Flag::X),
            InstructionType::REP => self.op_rep(&instr),
            InstructionType::SEP => self.op_sep(&instr),
            InstructionType::ADC => self.op_add(&instr),
            InstructionType::SBC => self.op_sbc(&instr),
            InstructionType::CMP => self.op_compare(&instr, Register::C, Flag::M),
            InstructionType::CPX => self.op_compare(&instr, Register::X, Flag::X),
            InstructionType::CPY => self.op_compare(&instr, Register::Y, Flag::X),
            InstructionType::DEC if instr.def.mode == AddressingMode::Accumulator => {
                self.op_incdec_reg(Register::C, -1, Flag::M)
            }
            InstructionType::DEX => self.op_incdec_reg(Register::X, -1, Flag::X),
            InstructionType::DEY => self.op_incdec_reg(Register::Y, -1, Flag::X),
            InstructionType::INC if instr.def.mode == AddressingMode::Accumulator => {
                self.op_incdec_reg(Register::C, 1, Flag::M)
            }
            InstructionType::INX => self.op_incdec_reg(Register::X, 1, Flag::X),
            InstructionType::INY => self.op_incdec_reg(Register::Y, 1, Flag::X),
            InstructionType::DEC => self.op_incdec(instr, -1),
            InstructionType::INC => self.op_incdec(instr, 1),
            InstructionType::AND => self.op_and(instr),
            InstructionType::EOR => self.op_eor(instr),
            InstructionType::ORA => self.op_ora(instr),
            InstructionType::BIT => self.op_bit(instr),
            InstructionType::TRB => self.op_trb(instr),
            InstructionType::TSB => self.op_tsb(instr),
            InstructionType::ASL if instr.def.mode == AddressingMode::Accumulator => {
                self.op_asl_acc()
            }
            InstructionType::ASL => self.op_asl(instr),
            InstructionType::LSR if instr.def.mode == AddressingMode::Accumulator => {
                self.op_lsr_acc()
            }
            InstructionType::LSR => self.op_lsr(instr),
            InstructionType::ROL if instr.def.mode == AddressingMode::Accumulator => {
                self.op_rol_acc()
            }
            InstructionType::ROL => self.op_rol(instr),
            InstructionType::ROR if instr.def.mode == AddressingMode::Accumulator => {
                self.op_ror_acc()
            }
            InstructionType::ROR => self.op_ror(instr),
            InstructionType::BCS => self.op_branch(instr, self.regs.test_flag(Flag::C)),
            InstructionType::BCC => self.op_branch(instr, !self.regs.test_flag(Flag::C)),
            InstructionType::BEQ => self.op_branch(instr, self.regs.test_flag(Flag::Z)),
            InstructionType::BNE => self.op_branch(instr, !self.regs.test_flag(Flag::Z)),
            InstructionType::BMI => self.op_branch(instr, self.regs.test_flag(Flag::N)),
            InstructionType::BPL => self.op_branch(instr, !self.regs.test_flag(Flag::N)),
            InstructionType::BVS => self.op_branch(instr, self.regs.test_flag(Flag::V)),
            InstructionType::BVC => self.op_branch(instr, !self.regs.test_flag(Flag::V)),
            InstructionType::BRA => self.op_branch(instr, true),
            InstructionType::BRL => self.op_branch_long(instr),
            InstructionType::JMP => self.op_jump(instr),

            _ => todo!(),
        }
    }

    /// Resolves an address from instruction data, registers, etc.
    /// based on the addressing mode and consumes cycles.
    fn resolve_address(
        &mut self,
        instr: &Instruction,
        abs_extra_cycle: bool,
        pb_extra_cycle: bool,
    ) -> Result<Address> {
        // Internal cycles for adding D, if applicable (D > 0).
        if self.regs.read(Register::DL) != 0 {
            match instr.def.mode {
                AddressingMode::Direct
                | AddressingMode::DirectPtr16
                | AddressingMode::DirectPtr16Y
                | AddressingMode::DirectPtr24
                | AddressingMode::DirectPtr24Y
                | AddressingMode::DirectXPtr16
                | AddressingMode::DirectX
                | AddressingMode::DirectY => self.tick_bus(1)?,
                _ => (),
            };
        }

        // Extra internal cycles for modes that add a register
        // to the address before resolution.
        match instr.def.mode {
            AddressingMode::DirectX
            | AddressingMode::DirectXPtr16
            | AddressingMode::DirectY
            | AddressingMode::StackS
            | AddressingMode::StackSPtr16Y => self.tick_bus(1)?,
            AddressingMode::AbsoluteX
            | AddressingMode::AbsoluteXPtr16
            | AddressingMode::AbsoluteY
                if abs_extra_cycle =>
            {
                self.tick_bus(1)?
            }
            _ => (),
        }

        let idx = |base: Address, idx: Address| {
            (
                base.wrapping_add(idx) & ADDRESS_MASK,
                base & !0xFF != base.wrapping_add(idx) & !0xFF & ADDRESS_MASK,
            )
        };
        let noidx = |base: Address| (base, false);
        let (address, crosses_page) = match instr.def.mode {
            AddressingMode::Direct => noidx(Address::from(
                self.regs
                    .read(Register::D)
                    .wrapping_add(instr.imm::<u16>()?),
            )),
            AddressingMode::DirectX => noidx(Address::from(
                self.regs
                    .read(Register::D)
                    .wrapping_add(instr.imm::<u16>()?)
                    .wrapping_add(self.regs.read(Register::X).into()),
            )),
            AddressingMode::DirectPtr16 => noidx(
                (Address::from(self.regs.read(Register::DBR)) << 16)
                    | Address::from(
                        self.read16_tick_a16(Address::from(
                            self.regs
                                .read(Register::D)
                                .wrapping_add(instr.imm::<u16>()?),
                        )),
                    ),
            ),
            AddressingMode::DirectPtr24 => noidx(Address::from(
                self.read24_tick_a16(Address::from(
                    self.regs
                        .read(Register::D)
                        .wrapping_add(instr.imm::<u16>()?),
                )),
            )),
            AddressingMode::DirectPtr16Y => idx(
                (Address::from(self.regs.read(Register::DBR)) << 16)
                    | Address::from(
                        self.read16_tick_a16(Address::from(
                            self.regs
                                .read(Register::D)
                                .wrapping_add(instr.imm::<u16>()?),
                        )),
                    ),
                self.regs.read(Register::Y).into(),
            ),
            AddressingMode::DirectPtr24Y => idx(
                Address::from(
                    self.read24_tick_a16(Address::from(
                        self.regs
                            .read(Register::D)
                            .wrapping_add(instr.imm::<u16>()?),
                    )),
                ),
                self.regs.read(Register::Y).into(),
            ),
            AddressingMode::DirectXPtr16 => noidx(
                (Address::from(self.regs.read(Register::DBR)) << 16)
                    | Address::from(
                        self.read16_tick_a16(Address::from(
                            self.regs
                                .read(Register::D)
                                .wrapping_add(instr.imm::<u16>()?)
                                .wrapping_add(self.regs.read(Register::X)),
                        )),
                    ),
            ),
            AddressingMode::DirectY => noidx(Address::from(
                self.regs
                    .read(Register::D)
                    .wrapping_add(instr.imm::<u16>()?)
                    .wrapping_add(self.regs.read(Register::Y)),
            )),
            AddressingMode::Absolute => noidx(
                Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(instr.imm::<u16>()?),
            ),
            AddressingMode::AbsoluteX => idx(
                Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(instr.imm::<u16>()?),
                self.regs.read(Register::X).into(),
            ),
            AddressingMode::AbsoluteY => idx(
                Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(instr.imm::<u16>()?),
                self.regs.read(Register::Y).into(),
            ),
            AddressingMode::AbsolutePtr16 => noidx(
                Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(self.read16_tick_a16(Address::from(instr.imm::<u16>()?))),
            ),
            AddressingMode::AbsoluteXPtr16 => noidx(Address::from(
                self.read16_tick_a16(
                    Address::from(self.regs.read(Register::K)) << 16
                        | Address::from(
                            instr
                                .imm::<u16>()?
                                .wrapping_add(self.regs.read(Register::X)),
                        ),
                ),
            )),
            AddressingMode::AbsolutePtr24 => noidx(Address::from(
                self.read24_tick_a16(Address::from(instr.imm::<u16>()?)),
            )),
            AddressingMode::StackS => noidx(Address::from(
                self.regs
                    .read(Register::S)
                    .wrapping_add(instr.imm::<u16>()?),
            )),
            AddressingMode::StackSPtr16Y => noidx(
                (Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(
                        self.read16_tick_a16(Address::from(
                            self.regs
                                .read(Register::S)
                                .wrapping_add(instr.imm::<u16>()?),
                        )),
                    ))
                .wrapping_add(self.regs.read(Register::Y).into())
                    & ADDRESS_MASK,
            ),
            AddressingMode::Long => noidx(instr.imm::<u32>()? & ADDRESS_MASK),
            AddressingMode::LongX => noidx(
                instr
                    .imm::<u32>()?
                    .wrapping_add(self.regs.read(Register::X).into())
                    & ADDRESS_MASK,
            ),
            AddressingMode::Relative8 => (
                (Address::from(self.regs.k) << 16)
                    | Address::from(
                        self.regs
                            .pc
                            .wrapping_add_signed(instr.imm::<u8>()? as i8 as i16),
                    ),
                false,
            ),
            AddressingMode::Relative16 => (
                (Address::from(self.regs.k) << 16)
                    | Address::from(self.regs.pc.wrapping_add_signed(instr.imm::<u16>()? as i16)),
                false,
            ),

            _ => todo!(),
        };

        if pb_extra_cycle {
            // Extra cycle if crossing a page boundary || !X
            match instr.def.mode {
                AddressingMode::AbsoluteX
                | AddressingMode::AbsoluteY
                | AddressingMode::DirectPtr16Y => {
                    if !self.regs.test_flag(Flag::X) || crosses_page {
                        self.tick_bus(1)?
                    }
                }
                _ => (),
            }
        } else {
            // More internal cycles for modes that do stuff AFTER
            // address resolution bus activity.
            match instr.def.mode {
                AddressingMode::DirectPtr16Y => self.tick_bus(1)?,
                _ => (),
            }
        }
        match instr.def.mode {
            AddressingMode::StackSPtr16Y => self.tick_bus(1)?,
            _ => (),
        }

        Ok(address)
    }

    /// Retrieves address and fetches instruction data.
    fn fetch_data(
        &mut self,
        instr: &Instruction,
        abs_extra_cycle: bool,
        pb_extra_cycle: bool,
        width_flag: Flag,
    ) -> Result<(u16, Address)> {
        Ok(match instr.def.mode {
            AddressingMode::Immediate8
            | AddressingMode::Immediate16
            | AddressingMode::ImmediateM
            | AddressingMode::ImmediateX => (instr.imm::<u16>()?, Address::MAX),
            _ => {
                let addr = self.resolve_address(instr, abs_extra_cycle, pb_extra_cycle)?;

                (
                    if self.regs.test_flag(width_flag) {
                        // 8-bit mode
                        self.read_tick(addr).into()
                    } else {
                        // 16-bit mode
                        match instr.def.mode {
                            AddressingMode::Direct
                            | AddressingMode::DirectX
                            | AddressingMode::StackS => self.read16_tick_a16(addr),
                            _ => self.read16_tick_a24(addr),
                        }
                    },
                    addr,
                )
            }
        })
    }
    /// CLx - Clear x
    fn op_clx(&mut self, f: Flag) -> Result<()> {
        self.regs.write_flags(&[(f, false)]);
        self.tick_bus(1)
    }

    /// SEx - Set x
    fn op_sex(&mut self, f: Flag) -> Result<()> {
        self.regs.write_flags(&[(f, true)]);
        self.tick_bus(1)
    }

    /// XCE - Exchange Carry and Emulation
    fn op_xce(&mut self) -> Result<()> {
        let c = self.regs.test_flag(Flag::C);
        self.regs.write_flags(&[(Flag::C, self.regs.emulation)]);
        self.regs.emulation = c;
        if self.regs.emulation {
            self.regs.write_flags(&[(Flag::M, true), (Flag::X, true)]);
            self.regs.write(Register::XH, 0);
            self.regs.write(Register::YH, 0);
            self.regs.write(Register::SH, 1);
        }
        self.tick_bus(1)
    }

    /// XBA - Exchange B and A
    fn op_xba(&mut self) -> Result<()> {
        let c = self.regs.read(Register::C);
        let result = c >> 8 | c << 8;
        self.regs.write(Register::C, result);

        self.regs
            .write_flags(&[(Flag::Z, result & 0xFF == 0), (Flag::N, result & 0x80 != 0)]);
        self.tick_bus(2)
    }

    /// Txx - Transfer some register to another register
    /// (always 16-bit)
    fn op_txx_16b(&mut self, from: Register, to: Register, flags: bool) -> Result<()> {
        let v = self.regs.read(from);
        self.regs.write(to, v);

        if flags {
            self.regs
                .write_flags(&[(Flag::N, v & 0x8000 != 0), (Flag::Z, v == 0)]);
        }
        self.tick_bus(1)
    }

    /// Txx - Transfer some register to another register
    /// (obeying X/M flags)
    fn op_txx(&mut self, from: Register, to: Register, flag: Flag) -> Result<()> {
        let v = self.regs.read(from);
        if self.regs.test_flag(flag) {
            // 8-bit mode
            self.regs
                .write(to, (self.regs.read(to) & 0xFF00) | (v & 0xFF));
            self.regs
                .write_flags(&[(Flag::N, v & 0x80 != 0), (Flag::Z, v & 0xFF == 0)]);
        } else {
            // 16-bit mode
            self.regs.write(to, v);
            self.regs
                .write_flags(&[(Flag::N, v & 0x8000 != 0), (Flag::Z, v == 0)]);
        }
        self.tick_bus(1)
    }

    /// STZ - Store zero
    fn op_stz(&mut self, instr: &Instruction) -> Result<()> {
        self.op_store(instr, 0, Flag::M)
    }

    /// STA/STX/STY - Store register
    fn op_stx_reg(&mut self, instr: &Instruction, reg: Register, flag: Flag) -> Result<()> {
        self.op_store(instr, self.regs.read(reg), flag)
    }

    /// Store operations
    fn op_store(&mut self, instr: &Instruction, value: u16, flag: Flag) -> Result<()> {
        let addr = self.resolve_address(instr, true, false)?;

        if self.regs.test_flag(flag) {
            self.write_tick(addr, value as u8);
        } else {
            self.write16_tick_a(instr, addr, value);
        }

        Ok(())
    }

    /// Load operations
    fn op_load(&mut self, instr: &Instruction, destreg: Register, flag: Flag) -> Result<()> {
        let (val, _) = self.fetch_data(instr, false, true, flag)?;

        if self.regs.test_flag(flag) {
            // 8-bit mode
            let regval = self.regs.read(destreg);
            self.regs.write(destreg, regval & 0xFF00 | val as u16);
            self.regs
                .write_flags(&[(Flag::N, val & 0x80 != 0), (Flag::Z, val == 0)]);
        } else {
            // 16-bit mode
            self.regs.write(destreg, val);
            self.regs
                .write_flags(&[(Flag::N, val & 0x8000 != 0), (Flag::Z, val == 0)]);
        }

        Ok(())
    }

    /// REP - REset P
    fn op_rep(&mut self, instr: &Instruction) -> Result<()> {
        let mask = instr.imm::<u8>()?;
        let p = self.regs.read8(Register::P);
        self.regs.write(Register::P, (p & !mask).try_into()?);
        self.tick_bus(1)?;
        Ok(())
    }

    /// SEP - SEt P
    fn op_sep(&mut self, instr: &Instruction) -> Result<()> {
        let mask = instr.imm::<u8>()?;
        let p = self.regs.read8(Register::P);
        self.regs.write(Register::P, (p | mask).try_into()?);
        self.tick_bus(1)?;
        Ok(())
    }

    /// ADC - Add (accumulator)
    fn op_add(&mut self, instr: &Instruction) -> Result<()> {
        let (data, _) = self.fetch_data(instr, false, true, Flag::M)?;

        let result = match (self.regs.test_flag(Flag::D), self.regs.test_flag(Flag::M)) {
            (true, false) => alu::add16_dec(
                self.regs.read(Register::C),
                data,
                self.regs.test_flag(Flag::C),
            ),
            (false, false) => alu::add16(
                self.regs.read(Register::C),
                data,
                self.regs.test_flag(Flag::C),
            ),
            (false, true) => alu::add8(
                self.regs.read(Register::A),
                data,
                self.regs.test_flag(Flag::C),
            ),
            (true, true) => alu::add8_dec(
                self.regs.read(Register::A),
                data,
                self.regs.test_flag(Flag::C),
            ),
        };

        if self.regs.test_flag(Flag::M) {
            // 8-bit
            self.regs.write(Register::A, result.result);
        } else {
            // 16-bit
            self.regs.write(Register::C, result.result);
        }
        self.regs.write_flags(&[
            (Flag::N, result.n),
            (Flag::V, result.v),
            (Flag::Z, result.z),
            (Flag::C, result.c),
        ]);

        Ok(())
    }

    /// SBC - SuBtract (accumulator)
    fn op_sbc(&mut self, instr: &Instruction) -> Result<()> {
        let (data, _) = self.fetch_data(instr, false, true, Flag::M)?;

        let result = match (self.regs.test_flag(Flag::D), self.regs.test_flag(Flag::M)) {
            (true, false) => alu::sub16_dec(
                self.regs.read(Register::C),
                data,
                self.regs.test_flag(Flag::C),
            ),
            (false, false) => alu::sub16(
                self.regs.read(Register::C),
                data,
                self.regs.test_flag(Flag::C),
            ),
            (false, true) => alu::sub8(
                self.regs.read(Register::A),
                data,
                self.regs.test_flag(Flag::C),
            ),
            (true, true) => alu::sub8_dec(
                self.regs.read(Register::A),
                data,
                self.regs.test_flag(Flag::C),
            ),
        };

        if self.regs.test_flag(Flag::M) {
            // 8-bit
            self.regs.write(Register::A, result.result);
        } else {
            // 16-bit
            self.regs.write(Register::C, result.result);
        }
        self.regs.write_flags(&[
            (Flag::N, result.n),
            (Flag::V, result.v),
            (Flag::Z, result.z),
            (Flag::C, result.c),
        ]);

        Ok(())
    }

    /// CMP/CPX/CPY - Compare
    fn op_compare(&mut self, instr: &Instruction, to: Register, width_flag: Flag) -> Result<()> {
        let (data, _) = self.fetch_data(instr, false, true, width_flag)?;
        let val = self.regs.read(to);

        let result = if self.regs.test_flag(width_flag) {
            alu::sub8(val & 0xFF, data & 0xFF, true)
        } else {
            alu::sub16(val, data, true)
        };

        self.regs.write_flags(&[
            (Flag::N, result.n),
            (Flag::C, result.c),
            (Flag::Z, result.z),
        ]);

        Ok(())
    }

    /// Increment/decrement (register variant)
    fn op_incdec_reg(&mut self, reg: Register, data: i32, width_flag: Flag) -> Result<()> {
        let val = self.regs.read(reg);
        if self.regs.test_flag(width_flag) {
            // 8-bit
            let result = (((val & 0xFF) as i32 + data) & 0xFF) as u16;
            self.regs.write(reg, val & 0xFF00 | result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);
        } else {
            // 16-bit
            let result = (val as i32 + data) as u16;
            self.regs.write(reg, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x8000 != 0)]);
        }

        self.tick_bus(1)
    }

    /// Increment/decrement (memory variant)
    fn op_incdec(&mut self, instr: &Instruction, data: i32) -> Result<()> {
        let (val, addr) = self.fetch_data(instr, true, false, Flag::M)?;

        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (((val & 0xFF) as i32 + data) & 0xFF) as u8;
            self.write_tick(addr, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);
        } else {
            // 16-bit
            let result = (val as i32 + data) as u16;
            self.write16_tick_a_desc(instr, addr, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x8000 != 0)]);
        }

        Ok(())
    }

    /// AND - Bitwise AND
    fn op_and(&mut self, instr: &Instruction) -> Result<()> {
        let val = self.regs.read(Register::C);
        let (data, _) = self.fetch_data(instr, false, true, Flag::M)?;
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = val & data & 0xFF;
            self.regs.write(Register::A, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);
        } else {
            // 16-bit
            let result = val & data;
            self.regs.write(Register::C, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x8000 != 0)]);
        }

        Ok(())
    }

    /// EOR - Bitwise Exclusive Or
    fn op_eor(&mut self, instr: &Instruction) -> Result<()> {
        let val = self.regs.read(Register::C);
        let (data, _) = self.fetch_data(instr, false, true, Flag::M)?;
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (val ^ data) & 0xFF;
            self.regs.write(Register::A, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);
        } else {
            // 16-bit
            let result = val ^ data;
            self.regs.write(Register::C, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x8000 != 0)]);
        }

        Ok(())
    }

    /// ORA - Bitwise Or
    fn op_ora(&mut self, instr: &Instruction) -> Result<()> {
        let val = self.regs.read(Register::C);
        let (data, _) = self.fetch_data(instr, false, true, Flag::M)?;
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (val | data) & 0xFF;
            self.regs.write(Register::A, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);
        } else {
            // 16-bit
            let result = val | data;
            self.regs.write(Register::C, result);
            self.regs
                .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x8000 != 0)]);
        }

        Ok(())
    }

    /// BIT - Test bits
    fn op_bit(&mut self, instr: &Instruction) -> Result<()> {
        let val = self.regs.read(Register::C);
        let (data, _) = self.fetch_data(instr, false, true, Flag::M)?;
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = val & data & 0xFF;

            match instr.def.mode {
                AddressingMode::ImmediateM => self.regs.write_flags(&[(Flag::Z, result == 0)]),
                _ => self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::N, data & 0x80 != 0),
                    (Flag::V, data & 0x40 != 0),
                ]),
            }
        } else {
            // 16-bit
            let result = val & data;
            match instr.def.mode {
                AddressingMode::ImmediateM => self.regs.write_flags(&[(Flag::Z, result == 0)]),
                _ => self.regs.write_flags(&[
                    (Flag::Z, result == 0),
                    (Flag::N, data & 0x8000 != 0),
                    (Flag::V, data & 0x4000 != 0),
                ]),
            }
        }

        Ok(())
    }

    /// TRB - Test and Reset bits
    fn op_trb(&mut self, instr: &Instruction) -> Result<()> {
        let val = self.regs.read(Register::C);
        let (data, addr) = self.fetch_data(instr, false, true, Flag::M)?;

        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = val as u8 & data as u8;
            self.regs.write_flags(&[(Flag::Z, result == 0)]);
            self.write_tick(addr, data as u8 & !val as u8);
        } else {
            // 16-bit
            let result = val & data;
            self.regs.write_flags(&[(Flag::Z, result == 0)]);
            self.write16_tick_a_desc(instr, addr, data & !val);
        }

        Ok(())
    }

    /// TSB - Test and Set bits
    fn op_tsb(&mut self, instr: &Instruction) -> Result<()> {
        let val = self.regs.read(Register::C);
        let (data, addr) = self.fetch_data(instr, false, true, Flag::M)?;

        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (val as u8) & (data as u8);
            self.regs.write_flags(&[(Flag::Z, result == 0)]);
            self.write_tick(addr, data as u8 | val as u8);
        } else {
            // 16-bit
            let result = val & data;
            self.regs.write_flags(&[(Flag::Z, result == 0)]);
            self.write16_tick_a_desc(instr, addr, data | val);
        }

        Ok(())
    }

    /// ASL - Arithmetic Shift Left (register variant)
    fn op_asl_acc(&mut self) -> Result<()> {
        let val = self.regs.read(Register::C);
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (val << 1) as u8;
            self.regs.write(Register::A, result.into());
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x80 != 0),
                (Flag::C, val & 0x80 != 0),
            ]);
        } else {
            // 16-bit
            let result = val << 1;
            self.regs.write(Register::C, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x8000 != 0),
                (Flag::C, val & 0x8000 != 0),
            ]);
        }

        self.tick_bus(1)
    }

    /// ASL - Arithmetic Shift Left (memory variant)
    fn op_asl(&mut self, instr: &Instruction) -> Result<()> {
        let (val, addr) = self.fetch_data(instr, true, false, Flag::M)?;

        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            let result = (val << 1) as u8;
            self.write_tick(addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x80 != 0),
                (Flag::C, val & 0x80 != 0),
            ]);
        } else {
            // 16-bit
            let result = val << 1;
            self.write16_tick_a_desc(instr, addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x8000 != 0),
                (Flag::C, val & 0x8000 != 0),
            ]);
        }

        Ok(())
    }

    /// LSR - Logical Shift Right (register variant)
    fn op_lsr_acc(&mut self) -> Result<()> {
        let val = self.regs.read(Register::C);
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (val as u8 >> 1) as u8;
            self.regs.write(Register::A, result.into());
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, false),
                (Flag::C, val & 0x01 != 0),
            ]);
        } else {
            // 16-bit
            let result = val >> 1;
            self.regs.write(Register::C, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, false),
                (Flag::C, val & 0x01 != 0),
            ]);
        }

        self.tick_bus(1)
    }

    /// LSR - Logical Shift Right (memory variant)
    fn op_lsr(&mut self, instr: &Instruction) -> Result<()> {
        let (val, addr) = self.fetch_data(instr, true, false, Flag::M)?;
        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            let result = (val as u8 >> 1) as u8;
            self.write_tick(addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, false),
                (Flag::C, val & 0x01 != 0),
            ]);
        } else {
            // 16-bit
            let result = val >> 1;
            self.write16_tick_a_desc(instr, addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, false),
                (Flag::C, val & 0x01 != 0),
            ]);
        }

        Ok(())
    }

    /// ROL - ROtate Left (register variant)
    fn op_rol_acc(&mut self) -> Result<()> {
        let val = self.regs.read(Register::C);
        let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = ((val << 1) | c) as u8;
            self.regs.write(Register::A, result.into());
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x80 != 0),
                (Flag::C, val & 0x80 != 0),
            ]);
        } else {
            // 16-bit
            let result = (val << 1) | c;
            self.regs.write(Register::C, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x8000 != 0),
                (Flag::C, val & 0x8000 != 0),
            ]);
        }

        self.tick_bus(1)
    }

    /// ROL - ROtate Left (memory variant)
    fn op_rol(&mut self, instr: &Instruction) -> Result<()> {
        let (val, addr) = self.fetch_data(instr, true, false, Flag::M)?;
        let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };

        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            let result = ((val << 1) | c) as u8;
            self.write_tick(addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x80 != 0),
                (Flag::C, val & 0x80 != 0),
            ]);
        } else {
            // 16-bit
            let result = (val << 1) | c;
            self.write16_tick_a_desc(instr, addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x8000 != 0),
                (Flag::C, val & 0x8000 != 0),
            ]);
        }

        Ok(())
    }

    /// ROR - ROtate Right (register variant)
    fn op_ror_acc(&mut self) -> Result<()> {
        let val = self.regs.read(Register::C);
        let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };
        if self.regs.test_flag(Flag::M) {
            // 8-bit
            let result = (((val & 0xFF) >> 1) | (c << 7)) as u8;
            self.regs.write(Register::A, result.into());
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x80 != 0),
                (Flag::C, val & 0x01 != 0),
            ]);
        } else {
            // 16-bit
            let result = (val >> 1) | (c << 15);
            self.regs.write(Register::C, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x8000 != 0),
                (Flag::C, val & 0x01 != 0),
            ]);
        }

        self.tick_bus(1)
    }

    /// ROR - ROtate Right (memory variant)
    fn op_ror(&mut self, instr: &Instruction) -> Result<()> {
        let (val, addr) = self.fetch_data(instr, true, false, Flag::M)?;
        let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };

        // Calculation cycle
        self.tick_bus(1)?;

        if self.regs.test_flag(Flag::M) {
            let result = (((val & 0xFF) >> 1) | (c << 7)) as u8;
            self.write_tick(addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x80 != 0),
                (Flag::C, val & 0x01 != 0),
            ]);
        } else {
            // 16-bit
            let result = (val >> 1) | (c << 15);
            self.write16_tick_a_desc(instr, addr, result);
            self.regs.write_flags(&[
                (Flag::Z, result == 0),
                (Flag::N, result & 0x8000 != 0),
                (Flag::C, val & 0x01 != 0),
            ]);
        }

        Ok(())
    }

    /// Branch operations
    fn op_branch(&mut self, instr: &Instruction, cc: bool) -> Result<()> {
        let addr = self.resolve_address(instr, false, false)?;
        if !cc {
            return Ok(());
        }

        self.regs.write(Register::PC, addr as u16);
        self.tick_bus(1)
    }

    /// Branch operations (24-bit)
    fn op_branch_long(&mut self, instr: &Instruction) -> Result<()> {
        let addr = self.resolve_address(instr, false, false)?;

        self.regs.write(Register::K, ((addr >> 16) as u8).into());
        self.regs.write(Register::PC, addr as u16);
        self.tick_bus(1)
    }

    /// Jump operations
    fn op_jump(&mut self, instr: &Instruction) -> Result<()> {
        let addr = self.resolve_address(instr, true, false)?;

        match instr.def.mode {
            AddressingMode::Long | AddressingMode::AbsolutePtr24 => {
                self.regs.write(Register::K, ((addr >> 16) as u8).into())
            }
            _ => (),
        }

        self.regs.write(Register::PC, addr as u16);
        Ok(())
    }
}
