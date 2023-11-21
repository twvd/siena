use anyhow::Result;

use crate::snes::bus::{Bus, BusIterator};
use crate::tickable::Ticks;

use super::instruction::{Instruction, InstructionType, Operand};
use super::regs::{Flag, Register, RegisterFile};

pub type SpcAddress = u16;
pub const SPC_ADDRESS_MASK: SpcAddress = 0xFFFF;

/// SNES audio CPU (SPC700)
pub struct CpuSpc700<TBus: Bus<SpcAddress>> {
    pub bus: TBus,
    pub regs: RegisterFile,
    pub cycles: Ticks,
}

impl<TBus> CpuSpc700<TBus>
where
    TBus: Bus<SpcAddress>,
{
    pub fn new(bus: TBus, reset_addr: u16) -> Self {
        Self {
            bus,
            regs: RegisterFile::from_pc(reset_addr),
            cycles: 0,
        }
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
        let mut busiter = BusIterator::new_from(&self.bus, self.regs.pc);
        Instruction::decode(&mut busiter)
    }

    /// Fetches and decodes the next instruction at PC
    pub fn fetch_next_instr(&mut self) -> Result<Instruction> {
        let mut fetched: Vec<u8> = vec![];

        for i in 0.. {
            let pc = self.regs.pc as SpcAddress;
            match Instruction::decode(&mut fetched.clone().into_iter()) {
                Err(_) => fetched.push(self.read_tick(pc.wrapping_add(i))),
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
    fn read_tick(&mut self, addr: SpcAddress) -> u8 {
        let v = self.bus.read(addr);
        self.tick_bus(1).unwrap();
        v
    }

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    fn read16_tick(&mut self, addr: SpcAddress) -> u16 {
        let mut v = self.bus.read(addr) as u16;
        self.tick_bus(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        v |= (self.bus.read(hi_addr) as u16) << 8;
        self.tick_bus(1).unwrap();
        v
    }

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 8-bits.
    fn read16_tick_a8(&mut self, addr: SpcAddress) -> u16 {
        let mut v = self.bus.read(addr) as u16;
        self.tick_bus(1).unwrap();
        let hi_addr = addr & 0xFF00 | SpcAddress::from((addr as u8).wrapping_add(1));
        v |= (self.bus.read(hi_addr) as u16) << 8;
        self.tick_bus(1).unwrap();
        v
    }

    /// Writes a memory location while ticking peripherals
    /// for the access time.
    fn write_tick(&mut self, addr: SpcAddress, val: u8) {
        self.bus.write(addr, val);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    fn write16_tick(&mut self, addr: SpcAddress, val: u16) {
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
        let hi_addr = addr.wrapping_add(1);
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Writes 16-bit (LE) to a memory location while ticking
    /// peripherals for the access time.
    /// Descending temporal order.
    fn write16_tick_desc(&mut self, addr: SpcAddress, val: u16) {
        let hi_addr = addr.wrapping_add(1);
        self.bus.write(hi_addr, (val >> 8) as u8);
        self.tick_bus(1).unwrap();
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
    }

    /// Pushes 8-bits onto the stack
    fn push8(&mut self, val: u8) {
        let addr = SpcAddress::from(self.regs.read_dec(Register::SP));
        self.write_tick(addr, val);
    }

    /// Pulls 8-bits from the stack
    fn pull8(&mut self) -> u8 {
        let addr = SpcAddress::from(self.regs.read_inc(Register::SP).wrapping_add(1));
        self.read_tick(addr)
    }

    /// Pushes 16-bits onto the stack, MSB-first
    fn push16(&mut self, val: u16) {
        self.push8((val >> 8) as u8);
        self.push8(val as u8);
    }

    /// Pulls 16-bits from the stack
    fn pull16(&mut self) -> u16 {
        let lo = self.pull8() as u16;
        let hi = self.pull8() as u16;
        lo | hi << 8
    }

    /// Executes an instruction.
    fn execute_instruction(&mut self, instr: &Instruction) -> Result<()> {
        match instr.def.instr_type {
            InstructionType::NOP => self.tick_bus(1),
            InstructionType::EI => {
                self.regs.write_flags(&[(Flag::I, true)]);
                self.tick_bus(2)
            }
            InstructionType::DI => {
                self.regs.write_flags(&[(Flag::I, false)]);
                self.tick_bus(2)
            }
            InstructionType::NOTC => {
                let c = self.regs.test_flag(Flag::C);
                self.regs.write_flags(&[(Flag::C, !c)]);
                self.tick_bus(2)
            }
            InstructionType::SETC => {
                self.regs.write_flags(&[(Flag::C, true)]);
                self.tick_bus(1)
            }
            InstructionType::SETP => {
                self.regs.write_flags(&[(Flag::P, true)]);
                self.tick_bus(1)
            }
            InstructionType::CLRP => {
                self.regs.write_flags(&[(Flag::P, false)]);
                self.tick_bus(1)
            }
            InstructionType::CLRC => {
                self.regs.write_flags(&[(Flag::C, false)]);
                self.tick_bus(1)
            }
            InstructionType::CLRV => {
                self.regs.write_flags(&[(Flag::V, false), (Flag::H, false)]);
                self.tick_bus(1)
            }
            InstructionType::SET1 => self.op_setclr1(instr, true),
            InstructionType::CLR1 => self.op_setclr1(instr, false),
            InstructionType::OR => self.op_or(instr),
            InstructionType::EOR => self.op_eor(instr),
            InstructionType::AND => self.op_and(instr),
            InstructionType::MOV => self.op_mov(instr, true),
            InstructionType::MOVNoFlags => self.op_mov(instr, false),
            _ => todo!(),
        }
    }

    /// Resolves a value from instruction data, registers, etc.
    /// Based on the addressing mode and consumes cycles.
    /// Returns the new index into immediate values and
    /// the resolved value.
    fn resolve_value(
        &mut self,
        instr: &Instruction,
        op_idx: usize,
        imm_idx: usize,
    ) -> Result<(usize, u8, Option<SpcAddress>)> {
        match instr.def.operands[op_idx] {
            Operand::Register(r) => Ok((imm_idx, self.regs.read8(r), None)),
            Operand::Immediate => Ok((imm_idx + 1, instr.imm8(imm_idx), None)),
            Operand::DirectPage | Operand::DirectPageBit(_) => {
                let addr = self.map_pageflag(instr.imm8(imm_idx));
                Ok((imm_idx + 1, self.read_tick(addr), Some(addr)))
            }
            Operand::DirectPageNoRead => {
                let addr = self.map_pageflag(instr.imm8(imm_idx));
                // Value is invalid (not read)
                Ok((imm_idx + 1, 0, Some(addr)))
            }
            Operand::DirectPageX => {
                // Internal cycle (for addition?)
                self.tick_bus(1)?;

                let addr = self.map_pageflag(
                    instr
                        .imm8(imm_idx)
                        .wrapping_add(self.regs.read8(Register::X)),
                );
                Ok((imm_idx + 1, self.read_tick(addr), Some(addr)))
            }
            Operand::DirectPageY => {
                // Internal cycle (for addition?)
                self.tick_bus(1)?;

                let addr = self.map_pageflag(
                    instr
                        .imm8(imm_idx)
                        .wrapping_add(self.regs.read8(Register::Y)),
                );
                Ok((imm_idx + 1, self.read_tick(addr), Some(addr)))
            }
            Operand::Absolute => {
                // The longest instruction is 3 bytes, so we can
                // safely assume this is always at index 0.
                assert_eq!(imm_idx, 0);

                let addr = instr.imm16();
                Ok((imm_idx + 2, self.read_tick(addr), Some(addr)))
            }
            Operand::AbsoluteX => {
                // The longest instruction is 3 bytes, so we can
                // safely assume this is always at index 0.
                assert_eq!(imm_idx, 0);

                // Internal cycle (for addition?)
                self.tick_bus(1)?;

                let addr = instr.imm16().wrapping_add(self.regs.read(Register::X));
                Ok((imm_idx + 2, self.read_tick(addr), Some(addr)))
            }
            Operand::AbsoluteY => {
                // The longest instruction is 3 bytes, so we can
                // safely assume this is always at index 0.
                assert_eq!(imm_idx, 0);

                // Internal cycle (for addition?)
                self.tick_bus(1)?;

                let addr = instr.imm16().wrapping_add(self.regs.read(Register::Y));
                Ok((imm_idx + 2, self.read_tick(addr), Some(addr)))
            }
            Operand::IndirectX => {
                // We don't use this, but it does happen..
                self.read_tick(self.regs.pc);

                let addr = self.map_pageflag(SpcAddress::from(self.regs.read8(Register::X)));
                Ok((imm_idx, self.read_tick(addr), Some(addr)))
            }
            Operand::IndirectXAutoInc => {
                self.tick_bus(1)?;

                let x = self.regs.read8_inc(Register::X);
                let addr = self.map_pageflag(SpcAddress::from(x));
                Ok((imm_idx, self.read_tick(addr), Some(addr)))
            }
            Operand::IndirectY => {
                let addr = self.map_pageflag(SpcAddress::from(self.regs.read8(Register::Y)));
                Ok((imm_idx, self.read_tick(addr), Some(addr)))
            }
            Operand::XIndexIndirect => {
                // Internal cycle (for addition?)
                self.tick_bus(1)?;

                let addr = self.read16_tick_a8(
                    self.map_pageflag(
                        instr
                            .imm8(imm_idx)
                            .wrapping_add(self.regs.read8(Register::X)),
                    ),
                );
                Ok((imm_idx + 1, self.read_tick(addr), Some(addr)))
            }
            Operand::IndirectYIndex => {
                // Internal cycle (for addition?)
                self.tick_bus(1)?;

                let addr = self
                    .read16_tick_a8(self.map_pageflag(instr.imm8(imm_idx)))
                    .wrapping_add(self.regs.read(Register::Y));
                Ok((imm_idx + 1, self.read_tick(addr), Some(addr)))
            }
            _ => todo!(),
        }
    }

    /// Translate 8-bit relative address from operands
    /// to a full address (P flag).
    fn map_pageflag<T: Into<SpcAddress>>(&self, addr: T) -> SpcAddress {
        if !self.regs.test_flag(Flag::P) {
            addr.into()
        } else {
            0x0100 | addr.into()
        }
    }

    /// SET1/CLR1
    fn op_setclr1(&mut self, instr: &Instruction, set: bool) -> Result<()> {
        let Operand::DirectPageBit(bit) = instr.def.operands[0] else {
            unreachable!()
        };
        let (_, val, Some(addr)) = self.resolve_value(instr, 0, 0)? else {
            unreachable!()
        };

        if set {
            self.write_tick(addr, val | (1 << bit));
        } else {
            self.write_tick(addr, val & !(1 << bit));
        }

        Ok(())
    }

    /// OR
    fn op_or(&mut self, instr: &Instruction) -> Result<()> {
        let (src_idx, val1, odest_addr) = self.resolve_value(instr, 0, 0)?;
        let (_, val2, _) = self.resolve_value(instr, 1, src_idx)?;

        let result = val1 | val2;
        match instr.def.operands[0] {
            Operand::Register(r) => self.regs.write(r, result as u16),
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs
            .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);

        Ok(())
    }

    /// EOR
    fn op_eor(&mut self, instr: &Instruction) -> Result<()> {
        let (src_idx, val1, odest_addr) = self.resolve_value(instr, 0, 0)?;
        let (_, val2, _) = self.resolve_value(instr, 1, src_idx)?;

        let result = val1 ^ val2;
        match instr.def.operands[0] {
            Operand::Register(r) => self.regs.write(r, result as u16),
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs
            .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);

        Ok(())
    }

    /// AND
    fn op_and(&mut self, instr: &Instruction) -> Result<()> {
        let (src_idx, val1, odest_addr) = self.resolve_value(instr, 0, 0)?;
        let (_, val2, _) = self.resolve_value(instr, 1, src_idx)?;

        let result = val1 & val2;
        match instr.def.operands[0] {
            Operand::Register(r) => self.regs.write(r, result as u16),
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs
            .write_flags(&[(Flag::Z, result == 0), (Flag::N, result & 0x80 != 0)]);

        Ok(())
    }

    /// MOV
    fn op_mov(&mut self, instr: &Instruction, flags: bool) -> Result<()> {
        let (src_idx, _, odest_addr) = self.resolve_value(instr, 0, 0)?;
        let (_, val, _) = self.resolve_value(instr, 1, src_idx)?;

        // Extra wait cycles
        match (instr.def.operands[0], instr.def.operands[1]) {
            (Operand::Register(_), Operand::Register(_))
            | (Operand::Register(_), Operand::IndirectXAutoInc) => self.tick_bus(1)?,
            _ => (),
        }

        match instr.def.operands[0] {
            Operand::Register(r) => self.regs.write(r, val as u16),
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, val)
                } else {
                    unreachable!()
                }
            }
        }

        if flags {
            self.regs
                .write_flags(&[(Flag::Z, val == 0), (Flag::N, val & 0x80 != 0)]);
        }

        Ok(())
    }
}
