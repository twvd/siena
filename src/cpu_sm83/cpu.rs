use anyhow::{bail, Result};
use std::borrow::Borrow;

use super::alu;
use super::instruction::{Instruction, Operand};
use super::instructions::InstructionType;
use super::regs::{Flag, Register, RegisterFile, RegisterWidth};
use crate::gameboy::bus::bus::{Bus, BusIterator, BusMember};
use crate::gameboy::tickable::{Ticks, ONE_MCYCLE};

/// CPU clock frequency (in Hz)
pub const CPU_CLOCK_HZ: usize = 4194304;

// Interrupt flags
pub const INT_VBLANK: u8 = 1 << 0;
pub const INT_LCDSTAT: u8 = 1 << 1;
pub const INT_TIMER: u8 = 1 << 2;
pub const INT_SERIAL: u8 = 1 << 3;
pub const INT_JOYPAD: u8 = 1 << 4;

// KEY1 bits
pub const KEY1_DOUBLE_SPEED: u8 = 1 << 7;
pub const KEY1_SWITCH: u8 = 1 << 0;

/// Return type of CPU::op_* functions
type CPUOpResult = Result<OpOk>;

/// Result of a successful CPU::op_* function.
pub struct OpOk {
    /// New program counter
    pc: u16,

    /// Cycles taken
    cycles: usize,
}

impl OpOk {
    /// Normal successful op, moves PC to next
    /// instruction and always fixed cycle count.
    #[inline(always)]
    fn ok(pc: u16, instr: &Instruction) -> Self {
        Self {
            pc: pc.wrapping_add(instr.len as u16),
            cycles: instr.def.cycles[0].into(),
        }
    }

    /// Branch op: successful op, branch not taken.
    #[inline(always)]
    fn no_branch(pc: u16, instr: &Instruction) -> Self {
        Self {
            pc: pc.wrapping_add(instr.len as u16),
            cycles: instr.def.cycles[1].into(),
        }
    }

    /// Branch op: successful op, branch taken.
    #[inline(always)]
    fn branch(pc: u16, instr: &Instruction) -> Self {
        Self {
            pc,
            cycles: instr.def.cycles[0].into(),
        }
    }
}

/// Gameboy CPU
pub struct CpuSm83<TBus: Bus> {
    /// Gameboy Color mode
    cgb: bool,

    /// Address bus
    pub bus: TBus,

    /// Register file
    pub regs: RegisterFile,

    /// Total amount of cycles
    cycles: usize,

    /// Memory access cycles for current instruction
    mem_cycles: usize,

    /// Interrupt Master Enable
    pub ime: bool,

    /// HALT instruction pauses CPU
    halted: bool,

    /// CGB KEY1 register
    key1: u8,

    /// EI instruction executed, enable IME after next step
    ei: bool,
}

impl<TBus> CpuSm83<TBus>
where
    TBus: Bus,
{
    /// IE register address on address bus
    const BUS_IE: u16 = 0xFFFF;

    /// IF register address on address bus
    const BUS_IF: u16 = 0xFF0F;

    /// Boot ROM disable register address on address bus
    const BUS_BOOTROM_DISABLE: u16 = 0xFF50;

    pub fn new(bus: TBus, cgb: bool) -> Self {
        let mut c = Self {
            cgb,
            bus,
            regs: RegisterFile::new(),
            cycles: 0,
            ime: false,
            halted: false,
            key1: 0,
            mem_cycles: 0,
            ei: false,
        };
        if c.read(Self::BUS_BOOTROM_DISABLE) == 1 {
            c.setup_postboot().unwrap();
        }
        c
    }

    /// Set up registers to the expected state after boot
    fn setup_postboot(&mut self) -> Result<()> {
        if self.cgb {
            self.regs.write(Register::A, 0x11)?;
        } else {
            self.regs.write(Register::A, 0x01)?;
        }
        self.regs.write(Register::F, 0xB0)?;
        self.regs.write(Register::B, 0x00)?;
        self.regs.write(Register::C, 0x13)?;
        self.regs.write(Register::D, 0x00)?;
        self.regs.write(Register::E, 0xD8)?;
        self.regs.write(Register::H, 0x01)?;
        self.regs.write(Register::H, 0x01)?;
        self.regs.write(Register::L, 0x4D)?;
        self.regs.write(Register::SP, 0xFFFE)?;
        self.regs.write(Register::PC, 0x0100)?;
        Ok(())
    }

    pub fn dump_state(&self) -> String {
        format!(
            "{} {} IME:{} IE:{} IF:{}\n --> {}\n",
            self.get_cycles(),
            self.regs,
            self.ime,
            self.read(Self::BUS_IE),
            self.read(Self::BUS_IF),
            self.peek_next_instr().unwrap()
        )
    }

    /// Fetches and decodes the next instruction at PC
    pub fn peek_next_instr(&self) -> Result<Instruction> {
        let mut busiter = BusIterator::new_from(self.bus.borrow(), self.regs.pc);
        Instruction::decode(&mut busiter)
    }

    /// Fetches and decodes the next instruction at PC
    pub fn fetch_next_instr(&mut self) -> Result<Instruction> {
        let mut fetched: Vec<u8> = vec![];

        for p in 0.. {
            match Instruction::decode(&mut fetched.clone().into_iter()) {
                Err(_) => fetched.push(self.read_tick(self.regs.pc.wrapping_add(p))),
                Ok(i) => return Ok(i),
            }
        }

        unreachable!()
    }

    fn service_interrupts(&mut self) -> Result<()> {
        if !self.ime && !self.halted {
            return Ok(());
        }

        let inte = self.read(Self::BUS_IE);
        let intf = self.read(Self::BUS_IF);
        let service = intf & inte;

        let mut calli = |addr, flag: u8| {
            // 2 wait states
            self.tick_bus(2 * ONE_MCYCLE)?;
            self.cycles += 2;

            self.halted = false;

            if self.ime {
                self.write(Self::BUS_IF, intf & !flag);
                self.stack_push(self.regs.pc);
                self.ime = false;
                self.regs.pc = addr;
            }

            Ok(())
        };

        if service & INT_VBLANK == INT_VBLANK {
            return calli(0x40, INT_VBLANK);
        }
        if service & INT_LCDSTAT == INT_LCDSTAT {
            return calli(0x48, INT_LCDSTAT);
        }
        if service & INT_TIMER == INT_TIMER {
            return calli(0x50, INT_TIMER);
        }
        if service & INT_SERIAL == INT_SERIAL {
            return calli(0x58, INT_SERIAL);
        }
        if service & INT_JOYPAD == INT_JOYPAD {
            return calli(0x60, INT_JOYPAD);
        }

        Ok(())
    }

    /// Executes one CPU step (one instruction).
    pub fn step(&mut self) -> Result<usize> {
        self.service_interrupts()?;

        if self.ei {
            // Setting IME is delayed by 1 instruction after EI.
            self.ime = true;
            self.ei = false;
        }

        if self.halted {
            // Make sure other peripherals at least stay awake during HALT.
            self.tick_bus_mcycle()?;
            return Ok(1);
        }

        // Execute the instruction.
        self.mem_cycles = 0;
        let instr = self.fetch_next_instr()?;
        let result = self.execute_instruction(&instr)?;
        self.regs.pc = result.pc;

        let Some(cycles_left) = result.cycles.checked_sub(self.mem_cycles) else {
            bail!("Consumed too many memory cycles");
        };

        if cycles_left > 0 {
            // Consume any additional cycles that were listed in the
            // definition but we did not consume with memory access or
            // deliberate delays.
            for _ in 0..(cycles_left / ONE_MCYCLE) {
                self.tick_bus_mcycle()?;
            }
            self.tick_bus(cycles_left % ONE_MCYCLE)?;
        }

        self.cycles += result.cycles;
        Ok(result.cycles)
    }

    fn execute_instruction(&mut self, instr: &Instruction) -> CPUOpResult {
        match instr.def.instr_type {
            InstructionType::NOP => self.op_nop(instr),
            InstructionType::LD => self.op_ld(instr),
            InstructionType::INC16 => self.op_inc_16b(instr),
            InstructionType::INC8 => self.op_inc_8b(instr),
            InstructionType::DEC16 => self.op_dec_16b(instr),
            InstructionType::DEC8 => self.op_dec_8b(instr),
            InstructionType::CCF => self.op_ccf(instr),
            InstructionType::SCF => self.op_scf(instr),
            InstructionType::HALT => self.op_halt(instr),
            InstructionType::ADD => self.op_add(instr),
            InstructionType::SET => self.op_set(instr),
            InstructionType::RES => self.op_res(instr),
            InstructionType::RLCA => self.op_rlca(instr),
            InstructionType::ADD16 => self.op_add_16b(instr),
            InstructionType::ADC => self.op_adc(instr),
            InstructionType::RST => self.op_rst(instr),
            InstructionType::RRCA => self.op_rrca(instr),
            InstructionType::STOP => self.op_stop(instr),
            InstructionType::RLA => self.op_rla(instr),
            InstructionType::JR => self.op_jr(instr),
            InstructionType::JR_NZ => self.op_jr_nz(instr),
            InstructionType::RRA => self.op_rra(instr),
            InstructionType::DAA => self.op_daa(instr),
            InstructionType::SBC => self.op_sbc(instr),
            InstructionType::SUB => self.op_sub(instr),
            InstructionType::DI => self.op_di(instr),
            InstructionType::EI => self.op_ei(instr),
            InstructionType::INVALID => self.op_invalid(instr),
            InstructionType::CP => self.op_cp(instr),
            InstructionType::RRC => self.op_rrc(instr),
            InstructionType::RLC => self.op_rlc(instr),
            InstructionType::RR => self.op_rr(instr),
            InstructionType::RL => self.op_rl(instr),
            InstructionType::SLA => self.op_sla(instr),
            InstructionType::SRA => self.op_sra(instr),
            InstructionType::SWAP => self.op_swap(instr),
            InstructionType::SRL => self.op_srl(instr),
            InstructionType::BIT => self.op_bit(instr),
            InstructionType::JRZ => self.op_jr_z(instr),
            InstructionType::CPL => self.op_cpl(instr),
            InstructionType::JRNC => self.op_jr_nc(instr),
            InstructionType::JRC => self.op_jr_c(instr),
            InstructionType::AND => self.op_and(instr),
            InstructionType::OR => self.op_or(instr),
            InstructionType::ADD_SP => self.op_add_sp(instr),
            InstructionType::XOR => self.op_xor(instr),
            InstructionType::PUSH => self.op_push(instr),
            InstructionType::POP => self.op_pop(instr),
            InstructionType::RET_NC => self.op_ret_nc(instr),
            InstructionType::JP_NC => self.op_jp_nc(instr),
            InstructionType::JP_C => self.op_jp_c(instr),
            InstructionType::RET_NZ => self.op_ret_nz(instr),
            InstructionType::RET_Z => self.op_ret_z(instr),
            InstructionType::RET => self.op_ret(instr),
            InstructionType::RETI => self.op_reti(instr),
            InstructionType::JP_NZ => self.op_jp_nz(instr),
            InstructionType::JP_Z => self.op_jp_z(instr),
            InstructionType::JP => self.op_jp(instr),
            InstructionType::CALL_NZ => self.op_call_nz(instr),
            InstructionType::CALL_NC => self.op_call_nc(instr),
            InstructionType::CALL_C => self.op_call_c(instr),
            InstructionType::RET_C => self.op_ret_c(instr),
            InstructionType::CALL_Z => self.op_call_z(instr),
            InstructionType::CALL => self.op_call(instr),
        }
    }

    pub fn get_cycles(&self) -> usize {
        self.cycles
    }

    /// Pushes 16-bits onto the stack.
    fn stack_push(&mut self, val: u16) {
        self.regs.sp = self.regs.sp.wrapping_sub(2);
        self.write16(self.regs.sp, val);
    }

    /// Pops 16-bits from the stack.
    fn stack_pop(&mut self) -> u16 {
        let val = self.read16_tick(self.regs.sp);
        self.regs.sp = self.regs.sp.wrapping_add(2);
        val
    }

    /// SET/RES generic implementation
    fn op_set_res(&mut self, instr: &Instruction, set: bool) -> CPUOpResult {
        // SET/RES const, _
        let Operand::Constant(bit) = instr.def.operands[0] else {
            bail!("Unknown first operand {:?}", instr.def.operands[0])
        };

        // This is always an 8-bit operation.
        assert!((0..8).contains(&bit));

        let val = match instr.def.operands[1] {
            // SET/RES _, reg
            Operand::Register(reg) => self.regs.read8(reg)?,
            // SET/RES _, (reg)
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            _ => unreachable!(),
        };

        let val = if set {
            val | (1 << bit)
        } else {
            val & !(1 << bit)
        };

        match instr.def.operands[1] {
            // SET/RES _, reg
            Operand::Register(reg) => self.regs.write8(reg, val)?,
            // SET/RES _, (reg)
            Operand::RegisterIndirect(reg) => self.write(self.regs.read16(reg)?, val),
            _ => unreachable!(),
        }

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// SET b,n - Set bit 'b' in 'n'
    pub fn op_set(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_set_res(instr, true)
    }

    /// RES b,n - Clear bit 'b' in 'n'
    pub fn op_res(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_set_res(instr, false)
    }

    /// SRL - Shift right
    pub fn op_srl(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let val = self.regs.read8(reg)?;
                let result = alu::shright_8b(val);
                self.regs.write8(reg, result.result)?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::shright_8b(val);
                self.write(addr, result.result);
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// SWAP - Swap nibbles
    pub fn op_swap(&mut self, instr: &Instruction) -> CPUOpResult {
        let val = match instr.def.operands[0] {
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::EightBit);
                self.regs.read8(reg)?
            }
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            _ => unreachable!(),
        };

        let val = (val >> 4) | ((val & 0x0F) << 4);

        match instr.def.operands[0] {
            Operand::Register(reg) => self.regs.write8(reg, val)?,
            Operand::RegisterIndirect(reg) => self.write(self.regs.read16(reg)?, val),
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::Z, val == 0),
            (Flag::N, false),
            (Flag::H, false),
            (Flag::C, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// SLA - Shift left
    pub fn op_sla(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let result = alu::shleft_8b(self.regs.read8(reg)?);
                self.regs.write8(reg, result.result)?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::shleft_8b(val);
                self.write(addr, result.result);
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// SRA - Shift right (most significant bit unchanged)
    pub fn op_sra(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let val = self.regs.read8(reg)?;
                let result = alu::shright_8b(val);
                self.regs.write8(reg, result.result | (val & 0x80))?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::shright_8b(val);
                self.write(addr, result.result | (val & 0x80));
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// BIT b,n - Test for bit 'b' in 'n'
    pub fn op_bit(&mut self, instr: &Instruction) -> CPUOpResult {
        // BIT const, _
        let Operand::Constant(bit) = instr.def.operands[0] else {
            bail!("Unknown first operand {:?}", instr.def.operands[0])
        };

        // This is always an 8-bit operation.
        assert!((0..8).contains(&bit));

        let val = match instr.def.operands[1] {
            // BIT _, reg
            Operand::Register(reg) => self.regs.read8(reg)?,
            // BIT _, (reg)
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::Z, (val & (1 << bit) == 0)),
            (Flag::H, true),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RL - Rotate left (carry = bit 9)
    pub fn op_rl(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let result = alu::rotleft_9b(self.regs.read8(reg)?, self.regs.test_flag(Flag::C));
                self.regs.write8(reg, result.result)?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::rotleft_9b(val, self.regs.test_flag(Flag::C));
                self.write(addr, result.result);
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RLA - Rotate Left, A register
    pub fn op_rla(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = alu::rotleft_9b(self.regs.read8(Register::A)?, self.regs.test_flag(Flag::C));
        self.regs.write8(Register::A, result.result)?;

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RLC - Rotate left (copy to carry)
    pub fn op_rlc(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let result = alu::rotleft_8b(self.regs.read8(reg)?);
                self.regs.write8(reg, result.result)?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::rotleft_8b(val);
                self.write(addr, result.result);
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RLCA - Rotate Left (copy to carry) - register A
    pub fn op_rlca(&mut self, instr: &Instruction) -> CPUOpResult {
        let reg = Register::A;
        let result = alu::rotleft_8b(self.regs.read8(reg)?);
        self.regs.write8(reg, result.result)?;

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RR - Rotate Right (bit 9 = carry)
    pub fn op_rr(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let result = alu::rotright_9b(self.regs.read8(reg)?, self.regs.test_flag(Flag::C));
                self.regs.write8(reg, result.result)?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::rotright_9b(val, self.regs.test_flag(Flag::C));
                self.write(addr, result.result);
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RRA - Rotate Right, Register A (bit 9 = carry)
    pub fn op_rra(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = alu::rotright_9b(self.regs.read8(Register::A)?, self.regs.test_flag(Flag::C));
        self.regs.write8(Register::A, result.result)?;

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RRC - Rotate Right (copy to carry)
    pub fn op_rrc(&mut self, instr: &Instruction) -> CPUOpResult {
        let result = match instr.def.operands[0] {
            Operand::Register(reg) => {
                let result = alu::rotright_8b(self.regs.read8(reg)?);
                self.regs.write8(reg, result.result)?;
                result
            }
            Operand::RegisterIndirect(reg) => {
                let addr = self.regs.read16(reg)?;
                let val = self.read_tick(addr);
                let result = alu::rotright_8b(val);
                self.write(addr, result.result);
                result
            }
            _ => unreachable!(),
        };

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, (result.result == 0)),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RRCA - Rotate Right (copy to carry) - register A
    pub fn op_rrca(&mut self, instr: &Instruction) -> CPUOpResult {
        let reg = Register::A;
        let result = alu::rotright_8b(self.regs.read8(reg)?);
        self.regs.write8(reg, result.result)?;

        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, false),
            (Flag::N, false),
            (Flag::Z, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// EI - Enable Interrupts
    pub fn op_ei(&mut self, instr: &Instruction) -> CPUOpResult {
        self.ei = true;
        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// DI - Disable Interrupts
    pub fn op_di(&mut self, instr: &Instruction) -> CPUOpResult {
        self.ime = false;
        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// RST - Call
    pub fn op_rst(&mut self, instr: &Instruction) -> CPUOpResult {
        // Internal delay
        self.tick_bus_mcycle()?;

        let next_addr = self.regs.pc.wrapping_add(instr.len as u16);
        self.stack_push(next_addr);

        let Operand::Constant(new_addr) = instr.def.operands[0] else {
            unreachable!()
        };

        Ok(OpOk::branch(new_addr.into(), instr))
    }

    /// NOP - No Operation
    pub fn op_nop(&mut self, instr: &Instruction) -> CPUOpResult {
        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// STOP 0 - Halts execution / speed switch strobe
    pub fn op_stop(&mut self, instr: &Instruction) -> CPUOpResult {
        if self.cgb && self.key1 & KEY1_SWITCH == KEY1_SWITCH {
            // Switch speeds
            self.key1 = (self.key1 ^ KEY1_DOUBLE_SPEED) & !KEY1_SWITCH;

            // The 'no branch' cycle cost is used for speed switch,
            // which takes 2050 cycles.
            Ok(OpOk::no_branch(self.regs.pc, instr))
        } else {
            // Normal STOP
            self.halted = true;
            Ok(OpOk::ok(self.regs.pc, instr))
        }
    }

    /// HALT - Halts execution until interrupt or reset
    pub fn op_halt(&mut self, instr: &Instruction) -> CPUOpResult {
        self.halted = true;

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// LD - Load
    pub fn op_ld(&mut self, instr: &Instruction) -> CPUOpResult {
        let indreg = |r: Register, regval| match r.width() {
            RegisterWidth::SixteenBit => regval,
            RegisterWidth::EightBit => {
                assert_eq!(regval & !0xFF, 0);
                0xFF00 | regval
            }
        };

        // Source operand
        let val: u16 = match &instr.def.operands[1] {
            // LD _, imm8
            Operand::Immediate8 => instr.imm8(1)?.into(),
            // LD _, (imm8)
            Operand::ImmediateIndirect8 => {
                self.read_tick(0xFF00_u16 | instr.imm8(1)? as u16).into()
            }
            // LD _, (imm16)
            Operand::ImmediateIndirect16 => self.read_tick(instr.imm16(1)?).into(),
            // LD _, imm16
            Operand::Immediate16 => instr.imm16(1)?,
            // LD _, reg
            Operand::Register(reg) => self.regs.read(*reg),
            // LD _, (reg)
            Operand::RegisterIndirect(reg) => {
                let addr = match reg.width() {
                    RegisterWidth::SixteenBit => self.regs.read16(*reg)?,
                    RegisterWidth::EightBit => 0xFF00 | self.regs.read8(*reg)? as u16,
                };

                self.read_tick(addr).into()
            }
            // LD _, (reg+)
            Operand::RegisterIndirectInc(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);

                let addr = self.regs.read_inc(*reg)?;
                self.read_tick(addr).into()
            }
            // LD _, (reg-)
            Operand::RegisterIndirectDec(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);

                let addr = self.regs.read_dec(*reg)?;
                self.read_tick(addr).into()
            }
            _ => unreachable!(),
        };

        // Destination operand
        match instr.def.operands[0] {
            // LD reg, _
            Operand::Register(dest) => self.regs.write(dest, val.try_into()?)?,
            // LD (reg), _
            Operand::RegisterIndirect(dest) => {
                let addr = self.regs.read(dest);
                self.write(indreg(dest, addr), val.try_into()?)
            }
            // LD (reg-), _
            Operand::RegisterIndirectDec(dest) => {
                let addr = self.regs.read_dec(dest)?;
                self.write(indreg(dest, addr), val.try_into()?)
            }
            // LD (reg+), _
            Operand::RegisterIndirectInc(dest) => {
                let addr = self.regs.read_inc(dest)?;
                self.write(indreg(dest, addr), val.try_into()?)
            }
            // LDH (a8), _
            Operand::ImmediateIndirect8 => {
                let addr = 0xFF00_u16 + instr.imm8(0)? as u16;
                self.write(addr, val.try_into()?)
            }
            // LDH (a16), _
            Operand::ImmediateIndirect16 => match instr.def.operands[1] {
                // LD (a16),SP writes low address first
                Operand::Register(Register::SP) => self.write16_acc_low(instr.imm16(0)?, val),
                Operand::Register(reg) if reg.width() == RegisterWidth::SixteenBit => {
                    unreachable!()
                }
                // ..everything else
                _ => self.write(instr.imm16(0)?, val.try_into()?),
            },
            _ => unreachable!(),
        }

        if instr.get_opcode() == 0xF9 {
            // Extra internal delay
            self.tick_bus_mcycle()?;
        }

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// LD HL,SP+r8 - Load,
    /// ADD SP, e
    /// These instructions behave the same, so combine them
    pub fn op_add_sp(&mut self, instr: &Instruction) -> CPUOpResult {
        let Operand::Register(dest) = instr.def.operands[0] else {
            unreachable!()
        };
        assert!(dest == Register::HL || dest == Register::SP);
        let sp = self.regs.sp;
        let rel = instr.imms8(1)? as i16;

        // Use the ALU 8-bit adder for just the flags.
        let res = alu::add_8b(sp as u8, rel as u8);
        self.regs.write(dest, sp.wrapping_add_signed(rel))?;
        self.regs.write_flags(&[
            (Flag::C, res.carry),
            (Flag::H, res.halfcarry),
            (Flag::Z, false),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// SCF - Set carry flag
    pub fn op_scf(&mut self, instr: &Instruction) -> CPUOpResult {
        self.regs
            .write_flags(&[(Flag::C, true), (Flag::N, false), (Flag::H, false)]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// CCF - Flip carry flag
    pub fn op_ccf(&mut self, instr: &Instruction) -> CPUOpResult {
        self.regs.write_flags(&[
            (Flag::C, !self.regs.test_flag(Flag::C)),
            (Flag::N, false),
            (Flag::H, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// CP - Compare
    pub fn op_cp(&mut self, instr: &Instruction) -> CPUOpResult {
        let val = match instr.def.operands[0] {
            // CP imm8
            Operand::Immediate8 => instr.imm8(0)?,
            // CP reg8
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::EightBit);
                self.regs.read8(reg)?
            }
            // CP (reg16)
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            _ => unreachable!(),
        };

        let result = alu::sub_8b(self.regs.read8(Register::A)?, val);
        self.regs.write_flags(&[
            (Flag::Z, result.result == 0),
            (Flag::H, result.halfcarry),
            (Flag::C, result.carry),
            (Flag::N, true),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// CPL - One's complement (of A)
    pub fn op_cpl(&mut self, instr: &Instruction) -> CPUOpResult {
        self.regs
            .write8(Register::A, !self.regs.read8(Register::A)?)?;
        self.regs.write_flags(&[(Flag::H, true), (Flag::N, true)]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// OR - Bitwise OR
    pub fn op_or(&mut self, instr: &Instruction) -> CPUOpResult {
        let a = self.regs.read8(Register::A)?;
        let val = match instr.def.operands[0] {
            // OR reg
            Operand::Register(r) => self.regs.read8(r)?,
            // OR (reg)
            Operand::RegisterIndirect(r) => {
                assert_eq!(r.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(r)?)
            }
            // OR imm8
            Operand::Immediate8 => instr.imm8(0)?,
            _ => unreachable!(),
        };
        let result = a | val;
        self.regs.write(Register::A, result.into())?;
        self.regs.write_flags(&[
            (Flag::Z, result == 0),
            (Flag::C, false),
            (Flag::H, false),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// XOR - Bitwise XOR
    pub fn op_xor(&mut self, instr: &Instruction) -> CPUOpResult {
        let a = self.regs.read8(Register::A)?;
        let val = match instr.def.operands[0] {
            // XOR reg
            Operand::Register(r) => self.regs.read8(r)?,
            // XOR imm8
            Operand::Immediate8 => instr.imm8(0)?,
            // XOR (reg)
            Operand::RegisterIndirect(r) => {
                assert_eq!(r.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(r)?)
            }
            _ => unreachable!(),
        };
        let result = a ^ val;
        self.regs.write(Register::A, result.into())?;
        self.regs.write_flags(&[
            (Flag::Z, result == 0),
            (Flag::C, false),
            (Flag::H, false),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// AND - Bitwise AND
    pub fn op_and(&mut self, instr: &Instruction) -> CPUOpResult {
        let a = self.regs.read8(Register::A)?;
        let val = match instr.def.operands[0] {
            // AND reg
            Operand::Register(r) => self.regs.read8(r)?,
            // AND (reg)
            Operand::RegisterIndirect(r) => {
                assert_eq!(r.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(r)?)
            }
            // AND imm8
            Operand::Immediate8 => instr.imm8(0)?,
            _ => unreachable!(),
        };
        let result = a & val;
        self.regs.write(Register::A, result.into())?;
        self.regs.write_flags(&[
            (Flag::Z, result == 0),
            (Flag::C, false),
            (Flag::H, true),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// PUSH - Push register onto stack
    pub fn op_push(&mut self, instr: &Instruction) -> CPUOpResult {
        let Operand::Register(reg) = instr.def.operands[0] else {
            unreachable!()
        };
        assert_eq!(reg.width(), RegisterWidth::SixteenBit);

        // Internal delay
        self.tick_bus_mcycle()?;

        self.stack_push(self.regs.read16(reg)?);
        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// POP - Pop register from stack
    pub fn op_pop(&mut self, instr: &Instruction) -> CPUOpResult {
        let Operand::Register(reg) = instr.def.operands[0] else {
            unreachable!()
        };
        assert_eq!(reg.width(), RegisterWidth::SixteenBit);

        let val = self.stack_pop();
        self.regs.write(reg, val)?;
        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// ADC - Add (8-bit) plus carry
    pub fn op_adc(&mut self, instr: &Instruction) -> CPUOpResult {
        // First operand is always A
        let left = match instr.def.operands[0] {
            Operand::Register(reg) => {
                assert_eq!(reg, Register::A);
                self.regs.read8(reg)?
            }
            _ => unreachable!(),
        };

        let right = match instr.def.operands[1] {
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::EightBit);
                self.regs.read8(reg)?
            }
            Operand::Immediate8 => instr.imm8(1)?,
            _ => unreachable!(),
        };

        let result = alu::add_8bc(left, right, self.regs.test_flag(Flag::C));
        self.regs.write8(Register::A, result.result)?;
        self.regs.write_flags(&[
            (Flag::Z, result.result == 0),
            (Flag::C, result.carry),
            (Flag::H, result.halfcarry),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// DAA - Decimal (BCD) adjust register A
    pub fn op_daa(&mut self, instr: &Instruction) -> CPUOpResult {
        let mut result = self.regs.read8(Register::A)?;
        let mut carry = self.regs.test_flag(Flag::C);

        if self.regs.test_flag(Flag::N) {
            if self.regs.test_flag(Flag::C) {
                result = result.wrapping_sub(0x60);
            }
            if self.regs.test_flag(Flag::H) {
                result = result.wrapping_sub(0x06);
            }
        } else {
            if self.regs.test_flag(Flag::C) || result > 0x99 {
                result = result.wrapping_add(0x60);
                carry = true;
            }
            if self.regs.test_flag(Flag::H) || (result & 0x0F) > 0x09 {
                result = result.wrapping_add(0x06);
            }
        }
        self.regs.write8(Register::A, result as u8)?;
        self.regs
            .write_flags(&[(Flag::C, carry), (Flag::H, false), (Flag::Z, result == 0)]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// ADD - Add (8-bit)
    pub fn op_add(&mut self, instr: &Instruction) -> CPUOpResult {
        // First operand is always A
        let left = match instr.def.operands[0] {
            Operand::Register(reg) => {
                assert_eq!(reg, Register::A);
                self.regs.read8(reg)?
            }
            _ => unreachable!(),
        };

        let right = match instr.def.operands[1] {
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::EightBit);
                self.regs.read8(reg)?
            }
            Operand::Immediate8 => instr.imm8(1)?,
            _ => unreachable!(),
        };

        let result = alu::add_8b(left, right);
        self.regs.write8(Register::A, result.result)?;
        self.regs.write_flags(&[
            (Flag::Z, result.result == 0),
            (Flag::C, result.carry),
            (Flag::H, result.halfcarry),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// ADD - Add (16-bit)
    pub fn op_add_16b(&mut self, instr: &Instruction) -> CPUOpResult {
        let left = match instr.def.operands[0] {
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.regs.read16(reg)?
            }
            _ => unreachable!(),
        };

        let right = match instr.def.operands[1] {
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.regs.read16(reg)?
            }
            _ => unreachable!(),
        };

        let result = alu::add_16b(left, right);

        let Operand::Register(destreg) = instr.def.operands[0] else {
            unreachable!()
        };
        self.regs.write(destreg, result.result)?;
        self.regs.write_flags(&[
            (Flag::C, result.carry),
            (Flag::H, result.halfcarry),
            (Flag::N, false),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// SUB - Subtract (8-bit)
    pub fn op_sub(&mut self, instr: &Instruction) -> CPUOpResult {
        let val: u8 = match instr.def.operands[0] {
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::EightBit);
                self.regs.read8(reg)?
            }
            Operand::RegisterIndirect(reg) => self.read_tick(self.regs.read16(reg)?),
            Operand::Immediate8 => instr.imm8(0)?,
            _ => unreachable!(),
        };

        let res = alu::sub_8b(self.regs.read8(Register::A)?, val);

        self.regs.write8(Register::A, res.result)?;
        self.regs.write_flags(&[
            (Flag::Z, (res.result == 0)),
            (Flag::N, true),
            (Flag::C, res.carry),
            (Flag::H, res.halfcarry),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// DEC - Decrement (8-bit)
    pub fn op_dec_8b(&mut self, instr: &Instruction) -> CPUOpResult {
        match instr.def.operands[0] {
            Operand::Register(reg) => {
                let res = alu::sub_8b(self.regs.read8(reg)?, 1);
                self.regs.write8(reg, res.result)?;
                self.regs.write_flags(&[
                    (Flag::H, res.halfcarry),
                    (Flag::N, true),
                    (Flag::Z, (res.result == 0)),
                    // Carry not used
                ]);
            }
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);

                let addr = self.regs.read16(reg)?;
                let res = alu::sub_8b(self.read_tick(addr), 1);
                self.write(addr, res.result);
                self.regs.write_flags(&[
                    (Flag::H, res.halfcarry),
                    (Flag::N, true),
                    (Flag::Z, (res.result == 0)),
                    // Carry not used
                ]);
            }
            _ => unreachable!(),
        }

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// DEC - Decrement (16-bit)
    pub fn op_dec_16b(&mut self, instr: &Instruction) -> CPUOpResult {
        let Operand::Register(reg) = instr.def.operands[0] else {
            unreachable!()
        };

        assert_eq!(reg.width(), RegisterWidth::SixteenBit);
        self.regs
            .write(reg, self.regs.read16(reg)?.wrapping_sub(1))?;

        // Internal delay
        self.tick_bus_mcycle()?;

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// INC - Increment (8-bit)
    pub fn op_inc_8b(&mut self, instr: &Instruction) -> CPUOpResult {
        match instr.def.operands[0] {
            Operand::Register(reg) => {
                let res = alu::add_8b(self.regs.read8(reg)?, 1);
                self.regs.write8(reg, res.result)?;
                self.regs.write_flags(&[
                    (Flag::H, res.halfcarry),
                    (Flag::N, false),
                    (Flag::Z, (res.result == 0)),
                    // Carry not used
                ]);
            }
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);

                let addr = self.regs.read16(reg)?;
                let res = alu::add_8b(self.read_tick(addr), 1);
                self.write(addr, res.result);
                self.regs.write_flags(&[
                    (Flag::H, res.halfcarry),
                    (Flag::N, false),
                    (Flag::Z, (res.result == 0)),
                    // Carry not used
                ]);
            }
            _ => unreachable!(),
        }

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// INC - Increment (16-bit)
    pub fn op_inc_16b(&mut self, instr: &Instruction) -> CPUOpResult {
        let Operand::Register(reg) = instr.def.operands[0] else {
            unreachable!()
        };

        assert_eq!(reg.width(), RegisterWidth::SixteenBit);
        self.regs
            .write(reg, self.regs.read16(reg)?.wrapping_add(1))?;

        // Internal delay
        self.tick_bus_mcycle()?;

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    /// JR _, s8 - Jump Relative (conditional/unconditional)
    fn op_jr_cc(&mut self, instr: &Instruction, cc: bool) -> CPUOpResult {
        if !cc {
            return Ok(OpOk::no_branch(self.regs.pc, instr));
        }

        // value = address - 2.
        let new_pc = self
            .regs
            .pc
            .wrapping_add_signed(instr.imms8(0)?.into())
            .wrapping_add(2);
        Ok(OpOk::branch(new_pc, instr))
    }

    /// JR s8 - Jump Relative (unconditionally)
    pub fn op_jr(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jr_cc(instr, true)
    }

    /// JR NC s8 - Jump Relative (if not carry)
    pub fn op_jr_nc(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jr_cc(instr, !self.regs.test_flag(Flag::C))
    }

    /// JR C s8 - Jump Relative (if carry)
    pub fn op_jr_c(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jr_cc(instr, self.regs.test_flag(Flag::C))
    }

    /// JR NZ s8 - Jump Relative (if not zero)
    pub fn op_jr_nz(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jr_cc(instr, !self.regs.test_flag(Flag::Z))
    }

    /// JR Z s8 - Jump Relative (if zero)
    pub fn op_jr_z(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jr_cc(instr, self.regs.test_flag(Flag::Z))
    }

    /// JP _ - Jump Absolute (conditional/unconditional)
    fn op_jp_cc(&mut self, instr: &Instruction, cc: bool) -> CPUOpResult {
        if !cc {
            return Ok(OpOk::no_branch(self.regs.pc, instr));
        }

        let new_pc = match instr.def.operands[0] {
            Operand::ImmediateIndirect16 => instr.imm16(0)?,
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.regs.read(reg)
            }
            _ => unreachable!(),
        };
        Ok(OpOk::branch(new_pc, instr))
    }

    /// JP _ - Jump Absolute (unconditionally)
    pub fn op_jp(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jp_cc(instr, true)
    }

    /// JP NC _ - Jump Absolute (if not carry)
    pub fn op_jp_nc(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jp_cc(instr, !self.regs.test_flag(Flag::C))
    }

    /// JP C _ - Jump Absolute (if carry)
    pub fn op_jp_c(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jp_cc(instr, self.regs.test_flag(Flag::C))
    }

    /// JP NZ _ - Jump Absolute (if not zero)
    pub fn op_jp_nz(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jp_cc(instr, !self.regs.test_flag(Flag::Z))
    }

    /// JP Z _ - Jump Absolute (if zero)
    pub fn op_jp_z(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_jp_cc(instr, self.regs.test_flag(Flag::Z))
    }

    /// CALL cc - Call (conditional/unconditional)
    fn op_call_cc(&mut self, instr: &Instruction, cc: bool) -> CPUOpResult {
        if !cc {
            return Ok(OpOk::no_branch(self.regs.pc, instr));
        }

        // Internal delay
        self.tick_bus_mcycle()?;

        let next_addr = self.regs.pc.wrapping_add(instr.len as u16);
        self.stack_push(next_addr);

        Ok(OpOk::branch(instr.imm16(0)?, instr))
    }

    /// CALL - Call (unconditional)
    pub fn op_call(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_call_cc(instr, true)
    }

    /// CALL - Call (if carry)
    pub fn op_call_c(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_call_cc(instr, self.regs.test_flag(Flag::C))
    }

    /// CALL - Call (if not carry)
    pub fn op_call_nc(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_call_cc(instr, !self.regs.test_flag(Flag::C))
    }

    /// CALL - Call (if not zero)
    pub fn op_call_nz(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_call_cc(instr, !self.regs.test_flag(Flag::Z))
    }

    /// CALL - Call (if zero)
    pub fn op_call_z(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_call_cc(instr, self.regs.test_flag(Flag::Z))
    }

    /// RET cc - Return (conditional/unconditional)
    fn op_ret_cc(&mut self, instr: &Instruction, cc: bool) -> CPUOpResult {
        if !cc {
            return Ok(OpOk::no_branch(self.regs.pc, instr));
        }

        let ret_addr = self.stack_pop();
        Ok(OpOk::branch(ret_addr, instr))
    }

    /// RET - Return (unconditional)
    pub fn op_ret(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_ret_cc(instr, true)
    }

    /// RET NC - Return (if not carry)
    pub fn op_ret_nc(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_ret_cc(instr, !self.regs.test_flag(Flag::C))
    }

    /// RET C - Return (if zero)
    pub fn op_ret_c(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_ret_cc(instr, self.regs.test_flag(Flag::C))
    }

    /// RET NZ - Return (if not zero)
    pub fn op_ret_nz(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_ret_cc(instr, !self.regs.test_flag(Flag::Z))
    }

    /// RET Z - Return (if zero)
    pub fn op_ret_z(&mut self, instr: &Instruction) -> CPUOpResult {
        self.op_ret_cc(instr, self.regs.test_flag(Flag::Z))
    }

    /// RETI - Return from interrupt
    pub fn op_reti(&mut self, instr: &Instruction) -> CPUOpResult {
        self.ime = true;
        self.op_ret_cc(instr, true)
    }

    /// SBC - Subtract (8-bit) minus carry
    pub fn op_sbc(&mut self, instr: &Instruction) -> CPUOpResult {
        // First operand is always A
        let left = match instr.def.operands[0] {
            Operand::Register(reg) => {
                assert_eq!(reg, Register::A);
                self.regs.read8(reg)?
            }
            _ => unreachable!(),
        };

        let right = match instr.def.operands[1] {
            Operand::RegisterIndirect(reg) => {
                assert_eq!(reg.width(), RegisterWidth::SixteenBit);
                self.read_tick(self.regs.read16(reg)?)
            }
            Operand::Register(reg) => {
                assert_eq!(reg.width(), RegisterWidth::EightBit);
                self.regs.read8(reg)?
            }
            Operand::Immediate8 => instr.imm8(1)?,
            _ => unreachable!(),
        };

        let result = alu::sub_8bc(left, right, self.regs.test_flag(Flag::C));
        self.regs.write8(Register::A, result.result)?;
        self.regs.write_flags(&[
            (Flag::Z, result.result == 0),
            (Flag::C, result.carry),
            (Flag::H, result.halfcarry),
            (Flag::N, true),
        ]);

        Ok(OpOk::ok(self.regs.pc, instr))
    }

    pub fn op_prefix_cb(&mut self, _instr: &Instruction) -> CPUOpResult {
        // Implemented as separate dispatch table
        unreachable!();
    }

    pub fn op_invalid(&mut self, instr: &Instruction) -> CPUOpResult {
        panic!(
            "Invalid opcode {:02X} @ PC {:04X} - {}",
            instr.raw[0],
            self.regs.pc,
            self.dump_state()
        );
    }

    fn is_double_speed(&self) -> bool {
        self.cgb && self.key1 & KEY1_DOUBLE_SPEED != 0
    }

    /// Tick peripherals
    fn tick_bus(&mut self, cycles: usize) -> Result<()> {
        if cycles == 0 {
            return Ok(());
        }

        let bus_ticks = Ticks::from_t_xs(cycles, self.is_double_speed());
        self.bus.tick(bus_ticks)
    }

    /// Tick peripherals for 1 M-cycle
    fn tick_bus_mcycle(&mut self) -> Result<()> {
        self.mem_cycles += ONE_MCYCLE;
        self.tick_bus(ONE_MCYCLE)
    }

    /// Reads a memory location while ticking peripherals
    /// for the access time.
    fn read_tick(&mut self, addr: u16) -> u8 {
        let v = self.read(addr);
        self.tick_bus_mcycle().unwrap();
        v
    }

    /// Reads a 16-bit memory location while ticking peripherals
    /// for the access time.
    fn read16_tick(&mut self, addr: u16) -> u16 {
        let l = self.read(addr);
        self.tick_bus_mcycle().unwrap();

        let h = self.read(addr.wrapping_add(1));
        self.tick_bus_mcycle().unwrap();

        l as u16 | (h as u16) << 8
    }
}

impl<TBus> BusMember for CpuSm83<TBus>
where
    TBus: Bus,
{
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            // KEY1 - Speed switch
            0xFF4D if self.cgb => self.key1,

            _ => self.bus.read(addr as u16),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            // KEY1 - Speed switch
            0xFF4D if self.cgb => self.key1 = self.key1 & !KEY1_SWITCH | (val & KEY1_SWITCH),

            _ => self.bus.write(addr as u16, val),
        }

        self.tick_bus_mcycle().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gameboy::bus::testbus::Testbus;

    type CPU = CpuSm83<Testbus>;

    fn cpu(code: &[u8]) -> CPU {
        let bus = Testbus::from(code);
        CpuSm83::new(bus, false)
    }

    fn cpu_cgb(code: &[u8]) -> CPU {
        let bus = Testbus::from(code);
        CpuSm83::new(bus, true)
    }

    fn cpu_run(cpu: &mut CPU) {
        cpu.step().unwrap();
    }

    fn run(code: &[u8]) -> CPU {
        let mut cpu = cpu(code);
        cpu_run(&mut cpu);
        cpu
    }

    fn run_cgb(code: &[u8]) -> CPU {
        let mut cpu = cpu_cgb(code);
        cpu_run(&mut cpu);
        cpu
    }

    fn run_reg(code: &[u8], reg: Register, val: u16) -> CPU {
        let mut cpu = cpu(code);
        cpu.regs.write(reg, val).unwrap();
        cpu_run(&mut cpu);
        cpu
    }

    fn run_flags(code: &[u8], flags: &[Flag]) -> CPU {
        let mut cpu = cpu(code);
        cpu.regs.write_flags(
            &flags
                .into_iter()
                .map(|&f| (f, true))
                .collect::<Vec<(Flag, bool)>>(),
        );
        cpu_run(&mut cpu);
        cpu
    }

    fn run_reg_flags(code: &[u8], reg: Register, val: u16, flags: &[Flag]) -> CPU {
        let mut cpu = cpu(code);
        cpu.regs.write(reg, val).unwrap();
        cpu.regs.write_flags(
            &flags
                .into_iter()
                .map(|&f| (f, true))
                .collect::<Vec<(Flag, bool)>>(),
        );
        cpu_run(&mut cpu);
        cpu
    }

    #[test]
    fn op_ld_reg_imm16() {
        let cpu = run(&[0x31, 0x34, 0x12]); // LD SP,0x1234
        assert_eq!(cpu.regs.sp, 0x1234);
    }

    #[test]
    fn op_ld_reg_imm8() {
        let cpu = run(&[0x3E, 0x12]); // LD A,0x12
        assert_eq!(cpu.regs.a, 0x12);
    }

    #[test]
    fn op_xor_reg() {
        let mut c = cpu(&[0xA8]); // XOR B
        c.regs.a = 0x55;
        c.regs.b = 0xAA;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0xA8]); // XOR B
        c.regs.a = 0xAA;
        c.regs.b = 0xAA;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_ld_indreg8_reg() {
        let mut c = cpu(&[0xE2]); // LD (C),A
        c.regs.c = 0x11;
        c.regs.a = 0x5A;
        cpu_run(&mut c);
        assert_eq!(c.read(0xFF11), 0x5A);
    }

    #[test]
    fn op_ld_reg_indreg8() {
        let mut c = cpu(&[0xF2]); // LD A,(C)
        c.regs.c = 0x11;
        c.write(0xFF11, 0x5A);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x5A);
    }

    #[test]
    fn op_ld_indreg16_reg() {
        let mut c = cpu(&[0x70]); // LD (HL),B
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.regs.b = 0x5A;
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0x5A);
    }

    #[test]
    fn op_ld_indreg16_dec_reg() {
        let mut c = cpu(&[0x32]); // LD (HL-),A
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.regs.a = 0x5A;
        cpu_run(&mut c);
        assert_eq!((c.regs.h, c.regs.l), (0x11, 0x21));
        assert_eq!(c.read(0x1122), 0x5A);
    }

    #[test]
    fn op_ld_indreg16_inc_reg() {
        let mut c = cpu(&[0x22]); // LD (HL+),A
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.regs.a = 0x5A;
        cpu_run(&mut c);
        assert_eq!((c.regs.h, c.regs.l), (0x11, 0x23));
        assert_eq!(c.read(0x1122), 0x5A);
    }

    #[test]
    fn op_ld_indimm8_reg() {
        let c = run_reg(&[0xE0, 0x5A], Register::A, 0x12);
        assert_eq!(c.read(0xFF5A), 0x12);
    }

    #[test]
    fn op_ld_indimm16_reg() {
        let c = run_reg(&[0xEA, 0x55, 0xAA], Register::A, 0x12);
        assert_eq!(c.read(0xAA55), 0x12);
    }

    #[test]
    fn op_ld_reg_reg() {
        let mut c = cpu(&[0x78]); // LD A,B
        c.regs.b = 0x55;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x55);
    }

    #[test]
    fn op_ld_reg_indreg16() {
        let mut c = cpu(&[0x1A]); // LD A,(DE)
        c.regs.write(Register::DE, 0x1122).unwrap();
        c.write(0x1122, 0x5A);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x5A);
    }

    #[test]
    fn op_ld_reg_indimm8() {
        let mut c = cpu(&[0xF0, 0x22]); // LD A,(imm8)
        c.write(0xFF22, 0x5A);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x5A);
    }

    #[test]
    fn op_set_reg() {
        let c = run(&[0xCB, 0xC7]); // SET 0,A
        assert_eq!(c.regs.a, 0x01);

        let c = run(&[0xCB, 0xFB]); // SET 7,E
        assert_eq!(c.regs.e, 0x80);

        let mut c = cpu(&[0xCB, 0xE2]); // SET 4,D
        c.regs.d = 0x0F;
        cpu_run(&mut c);
        assert_eq!(c.regs.d, 0x1F);
    }

    #[test]
    fn op_set_indreg() {
        let c = run_reg(&[0xCB, 0xC6], Register::HL, 0x1122); // SET 0,(HL)
        assert_eq!(c.read(0x1122), 0x01);
    }

    #[test]
    fn op_res_reg() {
        let mut c = cpu(&[0xCB, 0x80]); // RES 0,B
        c.regs.b = 0xFF;
        cpu_run(&mut c);
        assert_eq!(c.regs.b, 0xFE);

        let c = run(&[0xCB, 0x81]); // RES 0,C
        assert_eq!(c.regs.c, 0x00);
    }

    #[test]
    fn op_res_indreg() {
        let mut c = cpu(&[0xCB, 0x86]); // RES 0,(HL)
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xFF);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0xFE);
    }

    #[test]
    fn op_bit_reg() {
        let mut c = cpu(&[0xCB, 0x47]); // BIT 0,A
        c.regs.a = 0x01;
        cpu_run(&mut c);
        assert!(
            !c.regs.test_flag(Flag::Z) && c.regs.test_flag(Flag::H) && !c.regs.test_flag(Flag::N)
        );

        let c = run(&[0xCB, 0x61]); // BIT 4,C
        assert!(
            c.regs.test_flag(Flag::Z) && c.regs.test_flag(Flag::H) && !c.regs.test_flag(Flag::N)
        );
    }

    #[test]
    fn op_bit_indreg() {
        let mut c = cpu(&[0xCB, 0x46]); // BIT 0,(HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x01);
        cpu_run(&mut c);
        assert!(
            !c.regs.test_flag(Flag::Z) && c.regs.test_flag(Flag::H) && !c.regs.test_flag(Flag::N)
        );

        let c = run_reg(&[0xCB, 0x66], Register::HL, 0x1122); // BIT 4,(HL)
        assert!(
            c.regs.test_flag(Flag::Z) && c.regs.test_flag(Flag::H) && !c.regs.test_flag(Flag::N)
        );
    }

    #[test]
    fn op_jr() {
        let c = run(&[0x18, (-10_i8 - 2) as u8]); // JR -10
        assert_eq!(c.regs.pc, -10_i16 as u16);

        let c = run(&[0x18, 10 - 2]); // JR 10
        assert_eq!(c.regs.pc, 10);
    }

    #[test]
    fn op_jr_overflow() {
        // Regression test for overflow bug
        let c = run(&[0x18, 127]); // JR 127
        assert_eq!(c.regs.pc, 129);

        let c = run(&[0x18, 128]); // JR -128
        assert_eq!(c.regs.pc, 0u16.wrapping_sub(126));
    }

    #[test]
    fn op_jr_nz() {
        let c = run(&[0x20, 10 - 2]); // JR NZ 10
        assert_eq!(c.regs.pc, 10);
        assert_eq!(c.cycles, 12);

        let c = run_flags(
            &[0x20, 10 - 2], // JR NZ 10
            &[Flag::Z],
        );
        assert_ne!(c.regs.pc, 10);
        assert_eq!(c.cycles, 8);
    }

    #[test]
    fn op_jr_z() {
        let c = run(&[0x28, 10 - 2]); // JR Z 10
        assert_ne!(c.regs.pc, 10);
        assert_eq!(c.cycles, 8);

        let c = run_flags(
            &[0x28, 10 - 2], // JR Z 10
            &[Flag::Z],
        );
        assert_eq!(c.regs.pc, 10);
        assert_eq!(c.cycles, 12);
    }

    #[test]
    fn op_jr_nc() {
        let c = run(&[0x30, 10 - 2]); // JR NC 10
        assert_eq!(c.regs.pc, 10);
        assert_eq!(c.cycles, 12);

        let c = run_flags(
            &[0x30, 10 - 2], // JR NC 10
            &[Flag::C],
        );
        assert_ne!(c.regs.pc, 10);
        assert_eq!(c.cycles, 8);
    }

    #[test]
    fn op_jr_c() {
        let c = run(&[0x38, 10 - 2]); // JR C 10
        assert_ne!(c.regs.pc, 10);
        assert_eq!(c.cycles, 8);

        let c = run_flags(
            &[0x38, 10 - 2], // JR C 10
            &[Flag::C],
        );
        assert_eq!(c.regs.pc, 10);
        assert_eq!(c.cycles, 12);
    }

    #[test]
    fn op_inc_reg() {
        let c = run_reg(&[0x3C], Register::A, 0x00);
        assert_eq!(c.regs.a, 1);
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0x3C], Register::A, 0x0F);
        assert_eq!(c.regs.a, 0x10);
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0x3C], Register::A, 0xFF);
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_call() {
        let mut c = cpu(&[]);
        c.write_slice(&[0xCD, 0x34, 0x12], 0x8000);
        c.regs.pc = 0x8000;
        c.regs.sp = 0xFFFE;
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0x1234);
        assert_eq!(c.regs.sp, 0xFFFC);
        assert_eq!(c.read16(0xFFFC), 0x8003);

        // Wrapping past 0
        let c = run(&[0xCD, 0x34, 0x12]);
        assert_eq!(c.regs.pc, 0x1234);
        assert_eq!(c.regs.sp, 0xFFFE);
        assert_eq!(c.read16(0xFFFE), 0x0003);
    }

    #[test]
    fn op_call_c() {
        let c = run(&[0xDC, 0x34, 0x12]);
        assert_ne!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 12);

        let c = run_flags(&[0xDC, 0x34, 0x12], &[Flag::C]);
        assert_eq!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 24);
    }

    #[test]
    fn op_call_nc() {
        let c = run(&[0xD4, 0x34, 0x12]);
        assert_eq!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 24);

        let c = run_flags(&[0xD4, 0x34, 0x12], &[Flag::C]);
        assert_ne!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 12);
    }

    #[test]
    fn op_call_z() {
        let c = run(&[0xCC, 0x34, 0x12]);
        assert_ne!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 12);

        let c = run_flags(&[0xCC, 0x34, 0x12], &[Flag::Z]);
        assert_eq!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 24);
    }

    #[test]
    fn op_call_nz() {
        let c = run(&[0xC4, 0x34, 0x12]);
        assert_eq!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 24);

        let c = run_flags(&[0xC4, 0x34, 0x12], &[Flag::Z]);
        assert_ne!(c.regs.pc, 0x1234);
        assert_eq!(c.cycles, 12);
    }

    #[test]
    fn op_push() {
        let c = run_reg(&[0xC5], Register::BC, 0xABCD);
        assert_ne!(c.regs.sp, 0);
        assert_eq!(c.read16(c.regs.sp), 0xABCD);
    }

    #[test]
    fn op_pop() {
        let mut c = cpu(&[0xC1]);
        c.stack_push(0xABCD);
        assert_ne!(c.regs.sp, 0);
        cpu_run(&mut c);
        assert_eq!(c.regs.sp, 0);
        assert_eq!(c.regs.read16(Register::BC).unwrap(), 0xABCD);
    }

    #[test]
    fn op_rl_reg() {
        let c = run_reg(&[0xCB, 0x10], Register::B, 0x80); // RL B
        assert_eq!(c.regs.b, 0x00);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x10], Register::B, 0x40); // RL B
        assert_eq!(c.regs.b, 0x80);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x10], Register::B, 0x00); // RL B
        assert_eq!(c.regs.b, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rr_reg() {
        let c = run_reg(&[0xCB, 0x18], Register::B, 0x01); // RR B
        assert_eq!(c.regs.b, 0x00);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x18], Register::B, 0x40); // RR B
        assert_eq!(c.regs.b, 0x20);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x18], Register::B, 0x00); // RR B
        assert_eq!(c.regs.b, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rl_indreg() {
        let mut c = cpu(&[0xCB, 0x16]); // RL (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x40);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0x80);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rr_indreg() {
        let mut c = cpu(&[0xCB, 0x1E]); // RR (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x40);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0x20);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rla() {
        let c = run_reg(&[0x17], Register::A, 0x80);
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0x17], Register::A, 0x40);
        assert_eq!(c.regs.a, 0x80);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0x17], Register::A, 0x00);
        assert_eq!(c.regs.a, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg_flags(&[0x17], Register::A, 0x95, &[Flag::C]);
        assert_eq!(c.regs.a, 0x2B);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rra() {
        let c = run_reg(&[0x1F], Register::A, 0x01);
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0x1F], Register::A, 0x40);
        assert_eq!(c.regs.a, 0x20);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0x1F], Register::A, 0x00);
        assert_eq!(c.regs.a, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg_flags(&[0x1F], Register::A, 0x03, &[Flag::C]);
        assert_eq!(c.regs.a, 0x81);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rlc_reg() {
        let c = run_reg(&[0xCB, 0x00], Register::B, 0x80); // RLC B
        assert_eq!(c.regs.b, 0x01);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x00], Register::B, 0x40); // RLC B
        assert_eq!(c.regs.b, 0x80);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x00], Register::B, 0x00); // RLC B
        assert_eq!(c.regs.b, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rlc_indreg() {
        let mut c = cpu(&[0xCB, 0x06]); // RLC (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x80);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0x01);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rrc_reg() {
        let c = run_reg(&[0xCB, 0x08], Register::B, 0x01); // RRC B
        assert_eq!(c.regs.b, 0x80);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x08], Register::B, 0x80); // RRC B
        assert_eq!(c.regs.b, 0x40);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xCB, 0x08], Register::B, 0x00); // RRC B
        assert_eq!(c.regs.b, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rrc_indreg() {
        let mut c = cpu(&[0xCB, 0x0E]); // RRC (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x01);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0x80);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_dec_8b() {
        let c = run_reg(&[0x3D], Register::A, 0x00); // DEC A
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0x3D], Register::A, 0x01); // DEC A
        assert_eq!(c.regs.a, 0x00);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_dec_8b_ind() {
        let mut c = cpu(&[0x35]); // DEC (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0xFF);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let mut c = cpu(&[0x35]); // DEC (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 1);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_inc_16b() {
        let c = run_reg(&[0x23], Register::HL, 0x00);
        assert_eq!(c.regs.l, 0x01);
        assert_eq!(c.regs.h, 0x00);

        let c = run_reg(&[0x23], Register::HL, 0xFFFF);
        assert_eq!(c.regs.l, 0x00);
        assert_eq!(c.regs.h, 0x00);
    }

    #[test]
    fn op_ret() {
        let mut c = cpu(&[0xC9]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0xABCD);
    }

    #[test]
    fn op_reti() {
        let mut c = cpu(&[0xD9]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0xABCD);
        assert!(c.ime);
    }

    #[test]
    fn op_ret_z() {
        let mut c = cpu(&[0xC8]);
        c.regs.write_flags(&[(Flag::Z, true)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 20);

        let mut c = cpu(&[0xC8]);
        c.regs.write_flags(&[(Flag::Z, false)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_ne!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 8);
    }

    #[test]
    fn op_ret_nz() {
        let mut c = cpu(&[0xC0]);
        c.regs.write_flags(&[(Flag::Z, true)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_ne!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 8);

        let mut c = cpu(&[0xC0]);
        c.regs.write_flags(&[(Flag::Z, false)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 20);
    }

    #[test]
    fn op_ret_c() {
        let mut c = cpu(&[0xD8]);
        c.regs.write_flags(&[(Flag::C, true)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 20);

        let mut c = cpu(&[0xD8]);
        c.regs.write_flags(&[(Flag::C, false)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_ne!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 8);
    }

    #[test]
    fn op_ret_nc() {
        let mut c = cpu(&[0xD0]);
        c.regs.write_flags(&[(Flag::C, true)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_ne!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 8);

        let mut c = cpu(&[0xD0]);
        c.regs.write_flags(&[(Flag::C, false)]);
        c.stack_push(0xABCD);
        cpu_run(&mut c);
        assert_eq!(c.regs.pc, 0xABCD);
        assert_eq!(c.cycles, 20);
    }

    #[test]
    fn op_cp_imm8() {
        let c = run_reg(&[0xFE, 0x2F], Register::A, 0x3C);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xFE, 0x3C], Register::A, 0x3C);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xFE, 0x40], Register::A, 0x3C);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_cp_reg8() {
        let c = run_reg(&[0xB8], Register::B, 0x3C);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xB8], Register::B, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_cp_indreg16() {
        let mut c = cpu(&[0xBE]);
        (c.regs.h, c.regs.l) = (0x08, 0x00);
        c.write(0x0800, 0x3C);
        cpu_run(&mut c);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xBE], Register::HL, 0x0800);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_sub_reg8() {
        let mut c = cpu(&[0x90]); // SUB B
        c.regs.write8(Register::A, 0x3E).unwrap();
        c.regs.write8(Register::B, 0x3E).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0x90]); // SUB B
        c.regs.write8(Register::A, 0x3E).unwrap();
        c.regs.write8(Register::B, 0x0F).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x2F);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0x90]); // SUB B
        c.regs.write8(Register::A, 0x3E).unwrap();
        c.regs.write8(Register::B, 0x40).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xFE);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_sub_imm8() {
        let mut c = cpu(&[0xD6, 0x3E]); // SUB $3E
        c.regs.write8(Register::A, 0x3E).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_sub_indreg() {
        let mut c = cpu(&[0x96]); // SUB (HL)
        c.regs.write8(Register::A, 0x3E).unwrap();
        c.write(0x1122, 0x3E);
        c.regs.write(Register::HL, 0x1122).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_add_reg() {
        let mut c = cpu(&[0x80]); // ADD A,B
        c.regs.write8(Register::A, 0x3A).unwrap();
        c.regs.write8(Register::B, 0xC6).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0x80]); // ADD A,B
        c.regs.write8(Register::A, 0x3C).unwrap();
        c.regs.write8(Register::B, 0xFF).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x3B);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_add_indreg() {
        let mut c = cpu(&[0x86]); // ADD A,(HL)
        c.regs.write8(Register::A, 0x3A).unwrap();
        c.regs.write(Register::HL, 0x55AA).unwrap();
        c.write(0x55AA, 0xC6);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0x86]); // ADD A,(HL)
        c.regs.write8(Register::A, 0x3C).unwrap();
        c.regs.write(Register::HL, 0x55AA).unwrap();
        c.write(0x55AA, 0xFF);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x3B);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_add_imm8() {
        let c = run_reg(&[0xC6, 0xC6], Register::A, 0x3A); // ADD A, 0xC6
        assert_eq!(c.regs.a, 0);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xC6, 0xFF], Register::A, 0x3C); // ADD A, 0xFF
        assert_eq!(c.regs.a, 0x3B);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_halt() {
        let mut c = run(&[0x76]);
        assert!(c.halted);
        let pc = c.regs.pc;
        c.step().unwrap();
        assert_eq!(pc, c.regs.pc);
    }

    #[test]
    fn op_nop() {
        run(&[0x00]);
    }

    #[test]
    fn op_jp_imm16() {
        let c = run(&[0xC3, 0xBB, 0xAA]);
        assert_eq!(c.regs.pc, 0xAABB);
    }

    #[test]
    fn op_jp_indreg16() {
        let c = run_reg(&[0xE9], Register::HL, 0xAABB); // JP (HL)
        assert_eq!(c.regs.pc, 0xAABB);
    }

    #[test]
    fn op_jp_c_imm16() {
        let c = run(&[0xDA, 0xBB, 0xAA]);
        assert_ne!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 12);

        let c = run_flags(&[0xDA, 0xBB, 0xAA], &[Flag::C]);
        assert_eq!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 16);
    }

    #[test]
    fn op_jp_nc_imm16() {
        let c = run(&[0xD2, 0xBB, 0xAA]);
        assert_eq!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 16);

        let c = run_flags(&[0xD2, 0xBB, 0xAA], &[Flag::C]);
        assert_ne!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 12);
    }

    #[test]
    fn op_jp_z_imm16() {
        let c = run(&[0xCA, 0xBB, 0xAA]);
        assert_ne!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 12);

        let c = run_flags(&[0xCA, 0xBB, 0xAA], &[Flag::Z]);
        assert_eq!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 16);
    }

    #[test]
    fn op_jp_nz_imm16() {
        let c = run(&[0xC2, 0xBB, 0xAA]);
        assert_eq!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 16);

        let c = run_flags(&[0xC2, 0xBB, 0xAA], &[Flag::Z]);
        assert_ne!(c.regs.pc, 0xAABB);
        assert_eq!(c.cycles, 12);
    }

    #[test]
    fn op_ei() {
        let mut c = run(&[0xFB]);
        assert!(!c.ime);
        c.step().unwrap();
        assert!(c.ime);
    }

    #[test]
    fn op_di() {
        let mut c = cpu(&[0xF3]);
        c.ime = true;
        cpu_run(&mut c);
        assert!(!c.ime);
    }

    #[test]
    fn op_ld_reg_indreg16_inc() {
        let mut c = cpu(&[0x2A]); // LD A,(HL+)
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0x5A);
        cpu_run(&mut c);
        assert_eq!((c.regs.h, c.regs.l), (0x11, 0x23));
        assert_eq!(c.regs.a, 0x5A);
    }

    #[test]
    fn op_ld_reg_indreg16_dec() {
        let mut c = cpu(&[0x3A]); // LD A,(HL-)
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0x5A);
        cpu_run(&mut c);
        assert_eq!((c.regs.h, c.regs.l), (0x11, 0x21));
        assert_eq!(c.regs.a, 0x5A);
    }

    #[test]
    fn op_xor_indreg() {
        let mut c = cpu(&[0xAE]); // XOR (HL)
        c.regs.a = 0x55;
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAA);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0xAE]); // XOR (HL)
        c.regs.a = 0xAA;
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAA);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_xor_imm8() {
        let c = run_reg(&[0xEE, 0xAA], Register::A, 0x55); // XOR 0xAA
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xEE, 0xAA], Register::A, 0xAA); // XOR 0xAA
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_or_reg() {
        let mut c = cpu(&[0xB0]); // OR B
        c.regs.a = 0x55;
        c.regs.b = 0xAA;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0xB0]); // OR B
        c.regs.a = 0xAA;
        c.regs.b = 0xAA;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xAA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run(&[0xB0]); // OR B
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_or_imm8() {
        let c = run_reg(&[0xF6, 0xAA], Register::A, 0x55); // OR 0xAA
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xF6, 0xAA], Register::A, 0xAA); // OR 0xAA
        assert_eq!(c.regs.a, 0xAA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run(&[0xF6, 0x00]); // OR 0x00
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_or_indreg() {
        let mut c = cpu(&[0xB6]); // OR (HL)
        c.regs.a = 0x55;
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAA);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xFF);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0xB6]); // OR (HL)
        c.regs.a = 0xAA;
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAA);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xAA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_ld_reg_indimm16() {
        let mut c = cpu(&[0xFA, 0x22, 0x11]); // LD A,(imm16)
        c.write(0x1122, 0x5A);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x5A);
    }

    #[test]
    fn op_ld_hl_sp_e_pos() {
        let c = run_reg(&[0xF8, 0x10], Register::SP, 0x1000);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0x1010);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xF8, 0x0F], Register::SP, 0x1004);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0x1013);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xF8, 0x10], Register::SP, 0xFFFF);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0x000F);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_ld_hl_sp_e_neg() {
        let c = run_reg(&[0xF8, 0xF0], Register::SP, 0x1000);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0x0FF0);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xF8, 0xFD], Register::SP, 0x1004);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0x1001);
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xF8, 0xFF], Register::SP, 0x0000);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0xFFFF);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_add_sp_pos() {
        let c = run_reg(&[0xE8, 0x10], Register::SP, 0x1000); // ADD SP,16
        assert_eq!(c.regs.read16(Register::SP).unwrap(), 0x1010);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xE8, 0x0F], Register::SP, 0x1004); // ADD SP,15
        assert_eq!(c.regs.read16(Register::SP).unwrap(), 0x1013);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xE8, 0x10], Register::SP, 0xFFFF); // ADD SP,16
        assert_eq!(c.regs.read16(Register::SP).unwrap(), 0x000F);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_add_sp_neg() {
        let c = run_reg(&[0xE8, 0xF0], Register::SP, 0x1000); // ADD SP,-16
        assert_eq!(c.regs.read16(Register::SP).unwrap(), 0x0FF0);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xE8, 0xFD], Register::SP, 0x1004); // ADD SP,-3
        assert_eq!(c.regs.read16(Register::SP).unwrap(), 0x1001);
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));

        let c = run_reg(&[0xE8, 0xFF], Register::SP, 0x0000); // ADD SP,-1
        assert_eq!(c.regs.read16(Register::SP).unwrap(), 0xFFFF);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_and_reg() {
        let mut c = cpu(&[0xA0]); // AND B
        c.regs.a = 0x55;
        c.regs.b = 0xAA;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0xA0]); // AND B
        c.regs.a = 0xAA;
        c.regs.b = 0xAA;
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xAA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xA0], Register::A, 0xFF); // AND B
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_and_imm8() {
        let c = run_reg(&[0xE6, 0xAA], Register::A, 0x55); // AND 0xAA
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xE6, 0xAA], Register::A, 0xAA); // AND 0xAA
        assert_eq!(c.regs.a, 0xAA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg(&[0xE6, 0x00], Register::A, 0xAA); // AND 0x00
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_and_indreg() {
        let mut c = cpu(&[0xA6]); // AND (HL)
        c.regs.a = 0x55;
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAA);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let mut c = cpu(&[0xA6]); // OR (HL)
        c.regs.a = 0xAA;
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAA);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xAA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_dec_16b() {
        let c = run_reg(&[0x2B], Register::HL, 0x00); // DEC HL
        assert_eq!(c.regs.l, 0xFF);
        assert_eq!(c.regs.h, 0xFF);

        let c = run_reg(&[0x2B], Register::HL, 0xFFFF); // DEC HL
        assert_eq!(c.regs.l, 0xFE);
        assert_eq!(c.regs.h, 0xFF);
    }

    #[test]
    fn op_cpl() {
        let c = run_reg(&[0x2F], Register::A, 0x35);
        assert_eq!(c.regs.a, 0xCA);
        assert!(c.regs.test_flag(Flag::N));
        assert!(c.regs.test_flag(Flag::H));
    }

    #[test]
    fn op_swap_reg() {
        let c = run_reg(&[0xCB, 0x30], Register::B, 0xAB); // SWAP B
        assert_eq!(c.regs.b, 0xBA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));

        let c = run(&[0xCB, 0x30]); // SWAP B
        assert_eq!(c.regs.b, 0x00);
        assert!(c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
    }

    #[test]
    fn op_swap_indreg() {
        let mut c = cpu(&[0xCB, 0x36]); // SWAP (HL)
        (c.regs.h, c.regs.l) = (0x11, 0x22);
        c.write(0x1122, 0xAB);
        cpu_run(&mut c);
        assert_eq!(c.read(0x1122), 0xBA);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
    }

    #[test]
    fn op_rst() {
        let c = run(&[0xC7]); // RST 00H
        assert_eq!(c.regs.pc, 0x0000);
        assert_ne!(c.regs.sp, 0x0000);
        let c = run(&[0xD7]); // RST 10H
        assert_eq!(c.regs.pc, 0x0010);
        assert_ne!(c.regs.sp, 0x0000);
        let c = run(&[0xE7]); // RST 20H
        assert_eq!(c.regs.pc, 0x0020);
        assert_ne!(c.regs.sp, 0x0000);
        let c = run(&[0xF7]); // RST 30H
        assert_eq!(c.regs.pc, 0x0030);
        assert_ne!(c.regs.sp, 0x0000);

        let c = run(&[0xCF]); // RST 08H
        assert_eq!(c.regs.pc, 0x0008);
        assert_ne!(c.regs.sp, 0x0000);
        let c = run(&[0xDF]); // RST 18H
        assert_eq!(c.regs.pc, 0x0018);
        assert_ne!(c.regs.sp, 0x0000);
        let c = run(&[0xEF]); // RST 28H
        assert_eq!(c.regs.pc, 0x0028);
        assert_ne!(c.regs.sp, 0x0000);
        let c = run(&[0xFF]); // RST 38H
        assert_eq!(c.regs.pc, 0x0038);
        assert_ne!(c.regs.sp, 0x0000);
    }

    #[test]
    fn op_add_16b() {
        let mut c = cpu(&[0x09]); // ADD HL,BC
        c.regs.write(Register::HL, 0x8A23).unwrap();
        c.regs.write(Register::BC, 0x0605).unwrap();
        cpu_run(&mut c);
        assert_eq!(c.regs.read16(Register::HL).unwrap(), 0x9028);
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_scf() {
        let c = run(&[0x37]);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_ccf() {
        let c = run(&[0x3F]);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_flags(&[0x3F], &[Flag::C]);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_sla_reg() {
        let c = run_reg(&[0xCB, 0x20], Register::B, 0x84); // SLA B
        assert!(c.regs.test_flag(Flag::C));
        assert_eq!(c.regs.b, 0x08);
    }

    #[test]
    fn op_sla_indreg() {
        let mut c = cpu(&[0xCB, 0x26]); // SLA (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x82);
        cpu_run(&mut c);
        assert!(c.regs.test_flag(Flag::C));
        assert_eq!(c.read(0x1122), 0x04);
    }

    #[test]
    fn op_sra_reg() {
        let c = run_reg(&[0xCB, 0x28], Register::B, 0x81); // SRA B
        assert!(c.regs.test_flag(Flag::C));
        assert_eq!(c.regs.b, 0xC0);
    }

    #[test]
    fn op_sra_indreg() {
        let mut c = cpu(&[0xCB, 0x2E]); // SRA (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x82);
        cpu_run(&mut c);
        assert!(!c.regs.test_flag(Flag::C));
        assert_eq!(c.read(0x1122), 0xC1);
    }

    #[test]
    fn op_srl_reg() {
        let c = run_reg(&[0xCB, 0x38], Register::B, 0x81); // SRL B
        assert!(c.regs.test_flag(Flag::C));
        assert_eq!(c.regs.b, 0x40);
    }

    #[test]
    fn op_srl_indreg() {
        let mut c = cpu(&[0xCB, 0x3E]); // SRL (HL)
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x82);
        cpu_run(&mut c);
        assert!(!c.regs.test_flag(Flag::C));
        assert_eq!(c.read(0x1122), 0x41);
    }

    #[test]
    fn op_rlca() {
        let c = run_reg(&[0x07], Register::A, 0x85); // RLCA
        assert_eq!(c.regs.a, 0x0B);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_rrca() {
        let c = run_reg(&[0x0F], Register::A, 0x3B); // RRCA
        assert_eq!(c.regs.a, 0x9D);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::N));
        assert!(!c.regs.test_flag(Flag::Z));
    }

    #[test]
    fn op_adc_reg() {
        let mut c = cpu(&[0x88]); // ADC A,B
        c.regs.a = 0xE1;
        c.regs.b = 0x0F;
        c.regs.write_flags(&[(Flag::C, true)]);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xF1);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_adc_indreg() {
        let mut c = cpu(&[0x8E]); // ADC A,(HL)
        c.regs.a = 0xE1;
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x3B);
        c.regs.write_flags(&[(Flag::C, true)]);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x1D);
        assert!(c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(!c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_sbc_reg() {
        let mut c = cpu(&[0x98]); // SBC A,B
        c.regs.a = 0x3B;
        c.regs.b = 0x2A;
        c.regs.write_flags(&[(Flag::C, true)]);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0x10);
        assert!(!c.regs.test_flag(Flag::C));
        assert!(!c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_sbc_indreg() {
        let mut c = cpu(&[0x9E]); // SBC A,(HL)
        c.regs.a = 0x3B;
        c.regs.write(Register::HL, 0x1122).unwrap();
        c.write(0x1122, 0x4F);
        c.regs.write_flags(&[(Flag::C, true)]);
        cpu_run(&mut c);
        assert_eq!(c.regs.a, 0xEB);
        assert!(c.regs.test_flag(Flag::C));
        assert!(c.regs.test_flag(Flag::H));
        assert!(!c.regs.test_flag(Flag::Z));
        assert!(c.regs.test_flag(Flag::N));
    }

    #[test]
    fn op_daa() {
        let c = run_reg_flags(&[0x27], Register::A, 0x7D, &[]);
        assert_eq!(c.regs.a, 0x83);
        assert!(!c.regs.test_flag(Flag::N));

        let c = run_reg_flags(&[0x27], Register::A, 0x4B, &[Flag::N, Flag::H]);
        assert_eq!(c.regs.a, 0x45);
    }

    #[test]
    fn op_stop() {
        let c = run(&[0x10]); // STOP 0
        assert!(c.halted);

        // CGB has additional behaviour and is therefore tested seperately.
        let c = run_cgb(&[0x10]); // STOP 0
        assert!(c.halted);
    }

    #[test]
    fn interrupts() {
        fn test_int(iflag: u8, addr: u16) {
            let mut c = cpu(&[0x00]); // NOP
            c.ime = true;
            c.write(0xFFFF, iflag); // IE
            c.write(0xFF0F, iflag); // IF
            cpu_run(&mut c);
            assert_eq!(c.regs.pc, addr + 1);
            assert!(!c.ime);
            assert_eq!(c.read(0xFF0F), 0);
        }

        test_int(0x01, 0x40);
        test_int(0x02, 0x48);
        test_int(0x04, 0x50);
        test_int(0x08, 0x58);
        test_int(0x10, 0x60);
    }

    #[test]
    fn cgb_speed_switch() {
        let mut c = cpu_cgb(&[0x10]); // STOP 0
        assert_eq!(c.read(0xFF4D), 0);
        c.write(0xFF4D, 0x01);
        assert_eq!(c.read(0xFF4D), 0x01);
        let cycles = c.cycles;
        cpu_run(&mut c);
        assert!(!c.halted);
        assert_eq!(c.read(0xFF4D), 0x80);
        assert_eq!(c.cycles - cycles, 2050);

        // Test switching back
        c.regs.pc = 0x0000;
        assert_eq!(c.read(0xFF4D), 0x80);
        c.write(0xFF4D, 0x01);
        assert_eq!(c.read(0xFF4D), 0x81);
        let cycles = c.cycles;
        cpu_run(&mut c);
        assert!(!c.halted);
        assert_eq!(c.read(0xFF4D), 0x00);
        assert_eq!(c.cycles - cycles, 2050);
    }
}
