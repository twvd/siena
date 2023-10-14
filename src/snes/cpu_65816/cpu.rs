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
        let addr = self.resolve_address(instr)?;

        // Extra internal cycles
        if (instr.def.mode == AddressingMode::Direct || instr.def.mode == AddressingMode::DirectX)
            && self.regs.read(Register::DL) != 0
        {
            self.tick_bus(1)?;
        }
        if instr.def.mode == AddressingMode::DirectX || instr.def.mode == AddressingMode::AbsoluteX
        {
            self.tick_bus(1)?;
        }

        if self.regs.test_flag(Flag::M) {
            self.write_tick(addr, 0);
        } else {
            self.write16_tick_a16(addr, 0);
        }

        Ok(())
    }

    /// Resolves an address from instruction data, registers, etc.
    /// based on the addressing mode.
    fn resolve_address(&self, instr: &Instruction) -> Result<Address> {
        Ok(match instr.def.mode {
            AddressingMode::Direct => Address::from(
                self.regs
                    .read(Register::D)
                    .wrapping_add(instr.imm::<u16>()?),
            ),
            AddressingMode::DirectX => Address::from(
                self.regs
                    .read(Register::D)
                    .wrapping_add(instr.imm::<u16>()?)
                    .wrapping_add(self.regs.read(Register::X)),
            ),
            AddressingMode::Absolute => {
                Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(instr.imm::<u16>()?)
            }
            AddressingMode::AbsoluteX => {
                (Address::from(self.regs.read(Register::DBR)) << 16
                    | Address::from(instr.imm::<u16>()?))
                .wrapping_add(Address::from(self.regs.read(Register::X)))
                    & ADDRESS_MASK
            }

            _ => todo!(),
        })
    }
}
