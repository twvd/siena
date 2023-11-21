use anyhow::Result;
use num_traits::ToPrimitive;

use crate::snes::bus::{Bus, BusIterator};
use crate::tickable::Ticks;

use super::instruction::{Instruction, InstructionType, Operand};
use super::regs::{Flag, Register, RegisterFile, RegisterWidth};

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
            InstructionType::SET1 => self.op_setclr1(instr, true),
            InstructionType::CLR1 => self.op_setclr1(instr, false),
            _ => todo!(),
        }
    }

    fn as_directpage<T: Into<SpcAddress>>(&self, addr: T) -> SpcAddress {
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
        let addr = self.as_directpage(instr.immediate[0]);
        let val = self.read_tick(addr);

        if set {
            self.write_tick(addr, val | (1 << bit));
        } else {
            self.write_tick(addr, val & !(1 << bit));
        }

        Ok(())
    }
}
