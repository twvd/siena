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

    /// Reads 16-bits from a memory location while ticking
    /// peripherals for the access time.
    /// Address wraps at 8-bits, extra delay between reads.
    fn read16_tick_a8_delay(&mut self, addr: SpcAddress) -> u16 {
        let mut v = self.bus.read(addr) as u16;
        self.tick_bus(2).unwrap();
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
    /// 8-bit address wrap.
    fn write16_tick_a8(&mut self, addr: SpcAddress, val: u16) {
        self.bus.write(addr, (val & 0xFF) as u8);
        self.tick_bus(1).unwrap();
        let hi_addr = addr & 0xFF00 | SpcAddress::from((addr as u8).wrapping_add(1));
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
        let addr = SpcAddress::from(self.regs.read_dec(Register::SP) | 0x0100);
        self.write_tick(addr, val);
    }

    /// Pops 8-bits from the stack
    fn pop8(&mut self) -> u8 {
        let addr = SpcAddress::from(self.regs.read_inc(Register::SP).wrapping_add(1) | 0x0100);
        self.read_tick(addr)
    }

    /// Pushes 16-bits onto the stack, MSB-first
    fn push16(&mut self, val: u16) {
        self.push8((val >> 8) as u8);
        self.push8(val as u8);
    }

    /// Pops 16-bits from the stack
    fn pop16(&mut self) -> u16 {
        let lo = self.pop8() as u16;
        let hi = self.pop8() as u16;
        lo | hi << 8
    }

    /// Call to an address
    fn call(&mut self, addr: SpcAddress) {
        self.push16(self.regs.read(Register::PC));
        self.regs.write(Register::PC, addr);
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
            InstructionType::MOVW => self.op_movw(instr),
            InstructionType::ADC => self.op_adc(instr),
            InstructionType::SBC => self.op_sbc(instr),
            InstructionType::CMP => self.op_cmp(instr),
            InstructionType::DEC => self.op_dec(instr),
            InstructionType::INC => self.op_inc(instr),
            InstructionType::XCN => self.op_xcn(instr),
            InstructionType::ASL => self.op_asl(instr),
            InstructionType::LSR => self.op_lsr(instr),
            InstructionType::PUSH => self.op_push(instr),
            InstructionType::POP => self.op_pop(instr),
            InstructionType::CALL => {
                self.tick_bus(1)?;
                self.call(instr.imm16());
                self.tick_bus(2)
            }
            InstructionType::TCALL => self.op_tcall(instr),
            InstructionType::BBC => self.op_bbx(instr, false),
            InstructionType::BBS => self.op_bbx(instr, true),
            InstructionType::BCC => self.op_branch(instr, !self.regs.test_flag(Flag::C)),
            InstructionType::BCS => self.op_branch(instr, self.regs.test_flag(Flag::C)),
            InstructionType::BNE => self.op_branch(instr, !self.regs.test_flag(Flag::Z)),
            InstructionType::BEQ => self.op_branch(instr, self.regs.test_flag(Flag::Z)),
            InstructionType::BPL => self.op_branch(instr, !self.regs.test_flag(Flag::N)),
            InstructionType::BMI => self.op_branch(instr, self.regs.test_flag(Flag::N)),
            InstructionType::BVC => self.op_branch(instr, !self.regs.test_flag(Flag::V)),
            InstructionType::BVS => self.op_branch(instr, self.regs.test_flag(Flag::V)),
            InstructionType::BRA => self.op_branch(instr, true),
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
        match instr.def.operands {
            [Operand::Register(_), Operand::Register(_)]
            | [Operand::Register(_), Operand::IndirectXAutoInc] => self.tick_bus(1)?,
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

    /// MOVW
    fn op_movw(&mut self, instr: &Instruction) -> Result<()> {
        let val = match instr.def.operands[1] {
            Operand::Register(r) => self.regs.read(r),
            Operand::DirectPage => self.read16_tick_a8_delay(self.map_pageflag(instr.imm8(0))),
            _ => unreachable!(),
        };

        match instr.def.operands[0] {
            Operand::Register(r) => {
                self.regs.write(r, val);
                self.regs
                    .write_flags(&[(Flag::Z, val == 0), (Flag::N, val & 0x8000 != 0)]);
            }
            Operand::DirectPage => {
                let addr = self.map_pageflag(instr.imm8(0));

                // Reads LSB of destination, doesn't use it.
                self.read_tick(addr);

                self.write16_tick_a8(addr, val);
            }
            _ => unreachable!(),
        };

        Ok(())
    }

    /// ADC
    fn op_adc(&mut self, instr: &Instruction) -> Result<()> {
        let (src_idx, a, odest_addr) = self.resolve_value(instr, 0, 0)?;
        let (_, b, _) = self.resolve_value(instr, 1, src_idx)?;

        let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };
        let result = a as u16 + b as u16 + c as u16;
        let result_h = (((a & 0x0F) + (b & 0x0F) + c) & 0x10) == 0x10;
        let result_v = !(a ^ b) & (a ^ result as u8) & 0x80 != 0;

        match instr.def.operands[0] {
            Operand::Register(r) => self.regs.write(r, result & 0xFF),
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result as u8)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs.write_flags(&[
            (Flag::Z, (result & 0xFF) == 0),
            (Flag::N, result & 0x80 != 0),
            (Flag::C, result > u8::MAX.into()),
            (Flag::H, result_h),
            (Flag::V, result_v),
        ]);

        Ok(())
    }

    /// SBC
    fn op_sbc(&mut self, instr: &Instruction) -> Result<()> {
        let (src_idx, a, odest_addr) = self.resolve_value(instr, 0, 0)?;
        let (_, b, _) = self.resolve_value(instr, 1, src_idx)?;

        let c = if self.regs.test_flag(Flag::C) { 1 } else { 0 };
        let result = a as i16 + (!b) as i16 + c as i16;
        let result_h = (((a & 0x0F) + (!b & 0x0F) + c) & 0x10) == 0x10;
        let result_v = !(a ^ !b) & (a ^ result as u8) & 0x80 != 0;

        match instr.def.operands[0] {
            Operand::Register(r) => self.regs.write(r, (result & 0xFF) as u16),
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result as u8)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs.write_flags(&[
            (Flag::Z, (result & 0xFF) == 0),
            (Flag::N, result & 0x80 != 0),
            (Flag::C, result > u8::MAX.into()),
            (Flag::H, result_h),
            (Flag::V, result_v),
        ]);

        Ok(())
    }

    /// CMP
    fn op_cmp(&mut self, instr: &Instruction) -> Result<()> {
        let (src_idx, a, _) = self.resolve_value(instr, 0, 0)?;
        let (_, b, _) = self.resolve_value(instr, 1, src_idx)?;

        let result = (a as i16) - (b as i16);

        // Extra internal cycles
        match instr.def.operands {
            [Operand::DirectPage, Operand::DirectPage]
            | [Operand::DirectPage, Operand::Immediate]
            | [Operand::IndirectX, Operand::IndirectY] => self.tick_bus(1)?,
            _ => (),
        }

        self.regs.write_flags(&[
            (Flag::Z, (result & 0xFF) == 0),
            (Flag::N, result & 0x80 != 0),
            (Flag::C, result >= 0),
        ]);

        Ok(())
    }

    /// DEC
    fn op_dec(&mut self, instr: &Instruction) -> Result<()> {
        let (_, a, odest_addr) = self.resolve_value(instr, 0, 0)?;

        let result = a.wrapping_sub(1);

        match instr.def.operands[0] {
            Operand::Register(r) => {
                // Internal cycle
                self.tick_bus(1)?;
                self.regs.write(r, (result & 0xFF) as u16)
            }
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result as u8)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs.write_flags(&[
            (Flag::Z, (result & 0xFF) == 0),
            (Flag::N, result & 0x80 != 0),
        ]);

        Ok(())
    }

    /// INC
    fn op_inc(&mut self, instr: &Instruction) -> Result<()> {
        let (_, a, odest_addr) = self.resolve_value(instr, 0, 0)?;

        let result = a.wrapping_add(1);

        match instr.def.operands[0] {
            Operand::Register(r) => {
                // Internal cycle
                self.tick_bus(1)?;
                self.regs.write(r, (result & 0xFF) as u16)
            }
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result as u8)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs.write_flags(&[
            (Flag::Z, (result & 0xFF) == 0),
            (Flag::N, result & 0x80 != 0),
        ]);

        Ok(())
    }

    /// XCN
    fn op_xcn(&mut self, instr: &Instruction) -> Result<()> {
        let (_, a, _) = self.resolve_value(instr, 0, 0)?;

        // Internal cycles
        self.tick_bus(4)?;

        let result = (a >> 4) | (a << 4);
        self.regs.write(Register::A, result.into());

        self.regs.write_flags(&[
            (Flag::Z, (result & 0xFF) == 0),
            (Flag::N, result & 0x80 != 0),
        ]);

        Ok(())
    }

    /// ASL
    fn op_asl(&mut self, instr: &Instruction) -> Result<()> {
        let (_, val, odest_addr) = self.resolve_value(instr, 0, 0)?;

        let result = val << 1;

        match instr.def.operands[0] {
            Operand::Register(r) => {
                // Internal delay
                self.tick_bus(1)?;

                self.regs.write(r, result as u16)
            }
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs.write_flags(&[
            (Flag::Z, result == 0),
            (Flag::N, result & 0x80 != 0),
            (Flag::C, val & 0x80 != 0),
        ]);

        Ok(())
    }

    /// LSR
    fn op_lsr(&mut self, instr: &Instruction) -> Result<()> {
        let (_, val, odest_addr) = self.resolve_value(instr, 0, 0)?;

        let result = val >> 1;

        match instr.def.operands[0] {
            Operand::Register(r) => {
                // Internal delay
                self.tick_bus(1)?;

                self.regs.write(r, result as u16)
            }
            _ => {
                if let Some(dest_addr) = odest_addr {
                    self.write_tick(dest_addr, result)
                } else {
                    unreachable!()
                }
            }
        }

        self.regs.write_flags(&[
            (Flag::Z, result == 0),
            (Flag::N, result & 0x80 != 0),
            (Flag::C, val & 0x01 != 0),
        ]);

        Ok(())
    }

    /// PUSH
    fn op_push(&mut self, instr: &Instruction) -> Result<()> {
        let (_, val, _) = self.resolve_value(instr, 0, 0)?;

        // Internal delay
        self.tick_bus(1)?;

        self.push8(val);

        // Internal delay
        self.tick_bus(1)?;

        Ok(())
    }

    /// POP
    fn op_pop(&mut self, instr: &Instruction) -> Result<()> {
        let Operand::Register(reg) = instr.def.operands[0] else {
            unreachable!()
        };

        // Internal delays
        self.tick_bus(2)?;

        let val = self.pop8();
        self.regs.write(reg, val.into());
        Ok(())
    }

    /// TCALL
    fn op_tcall(&mut self, instr: &Instruction) -> Result<()> {
        let iaddr = match instr.def.operands[0] {
            Operand::ImpliedNum(0) => 0xFFDE,
            Operand::ImpliedNum(1) => 0xFFDC,
            Operand::ImpliedNum(2) => 0xFFDA,
            Operand::ImpliedNum(3) => 0xFFD8,
            Operand::ImpliedNum(4) => 0xFFD6,
            Operand::ImpliedNum(5) => 0xFFD4,
            Operand::ImpliedNum(6) => 0xFFD2,
            Operand::ImpliedNum(7) => 0xFFD0,
            Operand::ImpliedNum(8) => 0xFFCE,
            Operand::ImpliedNum(9) => 0xFFCC,
            Operand::ImpliedNum(10) => 0xFFCA,
            Operand::ImpliedNum(11) => 0xFFC8,
            Operand::ImpliedNum(12) => 0xFFC6,
            Operand::ImpliedNum(13) => 0xFFC4,
            Operand::ImpliedNum(14) => 0xFFC2,
            Operand::ImpliedNum(15) => 0xFFC0,
            _ => unreachable!(),
        };

        // Discarded read
        self.read_tick(self.regs.read(Register::PC));
        self.tick_bus(1)?;

        // Stack push happens before following the
        // address indirection.
        self.push16(self.regs.read(Register::PC));
        self.tick_bus(1)?;

        let addr = self.read16_tick(iaddr);
        self.regs.write(Register::PC, addr);
        Ok(())
    }

    /// BBC/BBS
    fn op_bbx(&mut self, instr: &Instruction, setclr: bool) -> Result<()> {
        // NOTE: Immediate values for these instructions are SWAPPED
        let (_, val, _) = self.resolve_value(instr, 0, 1)?;
        let Operand::DirectPageBit(bit) = instr.def.operands[0] else {
            unreachable!()
        };

        // Internal cycle
        self.tick_bus(1)?;

        if (val & (1 << bit) != 0) != setclr {
            // Branch not taken
            return Ok(());
        }

        // Branch taken
        let newpc = self
            .regs
            .read(Register::PC)
            .wrapping_add_signed(instr.imm8(0) as i8 as i16);
        self.regs.write(Register::PC, newpc);

        // Internal cycles
        self.tick_bus(2)?;
        Ok(())
    }

    /// (Conditional) branch instructions, except BBS/BBC
    fn op_branch(&mut self, instr: &Instruction, take: bool) -> Result<()> {
        if !take {
            // Branch not taken
            return Ok(());
        }

        // Branch taken
        let newpc = self
            .regs
            .read(Register::PC)
            .wrapping_add_signed(instr.imm8(0) as i8 as i16);
        self.regs.write(Register::PC, newpc);

        // Internal cycles
        self.tick_bus(2)?;
        Ok(())
    }
}
