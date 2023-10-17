use anyhow::Result;

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

        let _cycles = self.execute_instruction(&instr)?;
        self.regs.pc = self.regs.pc.wrapping_add(instr.len as u16);

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

    /// Writes 16-bit to a memory location while ticking
    /// peripherals for the access time.
    /// 16-bit address wrap.
    fn write16_tick_a16(&mut self, addr: Address, val: u16) {
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
        let hi_addr = addr & 0xFFFF0000 | Address::from((addr as u16).wrapping_add(1));
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit to a memory location while ticking
    /// peripherals for the access time.
    /// 24-bit address wrap.
    fn write16_tick_a24(&mut self, addr: Address, val: u16) {
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
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
            AddressingMode::AbsoluteX | AddressingMode::AbsoluteY if abs_extra_cycle => {
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
    ) -> Result<u16> {
        Ok(match instr.def.mode {
            AddressingMode::Immediate8
            | AddressingMode::Immediate16
            | AddressingMode::ImmediateM
            | AddressingMode::ImmediateX => instr.imm::<u16>()?,
            _ => {
                let addr = self.resolve_address(instr, abs_extra_cycle, pb_extra_cycle)?;

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
                }
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
            // Select different data address wraps
            match instr.def.mode {
                AddressingMode::Direct | AddressingMode::DirectX | AddressingMode::StackS => {
                    self.write16_tick_a16(addr, value)
                }
                _ => self.write16_tick_a24(addr, value),
            }
        }

        Ok(())
    }

    /// Load operations
    fn op_load(&mut self, instr: &Instruction, destreg: Register, flag: Flag) -> Result<()> {
        let val = self.fetch_data(instr, false, true, flag)?;

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
        let data = self.fetch_data(instr, false, true, Flag::M)?;

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
        let data = self.fetch_data(instr, false, true, Flag::M)?;

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
        let data = self.fetch_data(instr, false, true, width_flag)?;
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
}
