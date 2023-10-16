use anyhow::Result;

use crate::snes::bus::{Address, Bus, BusIterator, ADDRESS_MASK};
use crate::tickable::Ticks;

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
            InstructionType::LDA if instr.def.mode == AddressingMode::ImmediateM => {
                self.op_load_imm(&instr, Register::C, Flag::M)
            }
            InstructionType::LDX if instr.def.mode == AddressingMode::ImmediateX => {
                self.op_load_imm(&instr, Register::X, Flag::X)
            }
            InstructionType::LDY if instr.def.mode == AddressingMode::ImmediateX => {
                self.op_load_imm(&instr, Register::Y, Flag::X)
            }

            InstructionType::LDA => self.op_load(&instr, Register::C, Flag::M),
            InstructionType::LDX => self.op_load(&instr, Register::X, Flag::X),
            InstructionType::LDY => self.op_load(&instr, Register::Y, Flag::X),

            _ => todo!(),
        }
    }

    /// Resolves an address from instruction data, registers, etc.
    /// based on the addressing mode.
    /// Also returns if a page boundary was crossed for indexed
    /// modes where this is relevant.
    fn resolve_address(&mut self, instr: &Instruction) -> Result<(Address, bool)> {
        let idx = |base: Address, idx: Address| {
            (
                base.wrapping_add(idx) & ADDRESS_MASK,
                base & !0xFF != base.wrapping_add(idx) & !0xFF & ADDRESS_MASK,
            )
        };
        let noidx = |base: Address| (base, false);

        Ok(match instr.def.mode {
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
            | AddressingMode::AbsoluteX
            | AddressingMode::AbsoluteY
            | AddressingMode::StackS
            | AddressingMode::StackSPtr16Y => self.tick_bus(1)?,
            _ => (),
        }

        // Resolve address now to have the bus activity right at
        // this point.
        let (addr, _) = self.resolve_address(instr)?;

        // More internal cycles for modes that do stuff AFTER
        // address resolution bus activity.
        match instr.def.mode {
            AddressingMode::DirectPtr16Y | AddressingMode::StackSPtr16Y => self.tick_bus(1)?,
            _ => (),
        }

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
            _ => (),
        }

        // Resolve address now to have the bus activity right at
        // this point.
        let (addr, crosses_page) = self.resolve_address(instr)?;

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

        // More internal cycles for modes that do stuff AFTER
        // address resolution bus activity.
        match instr.def.mode {
            AddressingMode::StackSPtr16Y => self.tick_bus(1)?,
            _ => (),
        }

        if self.regs.test_flag(flag) {
            // 8-bit mode
            let val = self.read_tick(addr);
            let regval = self.regs.read(destreg);
            self.regs.write(destreg, regval & 0xFF00 | val as u16);
            self.regs
                .write_flags(&[(Flag::N, val & 0x80 != 0), (Flag::Z, val == 0)]);
        } else {
            // 16-bit mode
            let val = match instr.def.mode {
                AddressingMode::Direct | AddressingMode::DirectX | AddressingMode::StackS => {
                    self.read16_tick_a16(addr)
                }
                _ => self.read16_tick_a24(addr),
            };

            self.regs.write(destreg, val);
            self.regs
                .write_flags(&[(Flag::N, val & 0x8000 != 0), (Flag::Z, val == 0)]);
        }

        Ok(())
    }

    /// Load operations (immediate addressing mode)
    fn op_load_imm(&mut self, instr: &Instruction, destreg: Register, flag: Flag) -> Result<()> {
        if self.regs.test_flag(flag) {
            // 8-bit mode
            let val = instr.imm::<u8>()?;
            let regval = self.regs.read(destreg);
            self.regs.write(destreg, regval & 0xFF00 | val as u16);
            self.regs
                .write_flags(&[(Flag::N, val & 0x80 != 0), (Flag::Z, val == 0)]);
        } else {
            // 16-bit mode
            let val = instr.imm::<u16>()?;
            self.regs.write(destreg, val);
            self.regs
                .write_flags(&[(Flag::N, val & 0x8000 != 0), (Flag::Z, val == 0)]);
        }

        Ok(())
    }
}
