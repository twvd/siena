use anyhow::Result;

use crate::snes::bus::{Address, Bus, BusIterator};
use crate::tickable::Ticks;

use super::instruction::{Instruction, InstructionType};
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

        // TODO program bank?
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
            InstructionType::TCD => self.op_txx(Register::C, Register::D, true),
            InstructionType::TCS => self.op_txx(Register::C, Register::S, false),
            InstructionType::TDC => self.op_txx(Register::D, Register::C, true),
            InstructionType::TSC => self.op_txx(Register::S, Register::C, true),
            _ => todo!(),
        }
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
    fn op_txx(&mut self, from: Register, to: Register, flags: bool) -> Result<()> {
        let v = self.regs.read(from);
        self.regs.write(to, v);

        if flags {
            self.regs
                .write_flags(&[(Flag::N, v & 0x8000 != 0), (Flag::Z, v == 0)]);
        }
        self.tick_bus(1)
    }
}
