use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::tickable::Ticks;

use super::instruction::*;
use super::regs::{Flag, Flags, Register, RegisterFile, SR};

/// NEC uPD77C25
#[derive(Serialize, Deserialize)]
pub struct CpuUpd77c25 {
    pub regs: RegisterFile,
    pub cycles: Ticks,
    pub rodata: Vec<u8>,
    pub code: Vec<u8>,
    pub ram: Vec<u8>,
    pub stack: Vec<u16>,
}

impl CpuUpd77c25 {
    pub fn new() -> Self {
        Self {
            regs: RegisterFile::from_pc(0),
            cycles: 0,
            code: vec![0xFF; 16 * 1024],
            rodata: vec![0; 2048],
            ram: vec![0; 2048],
            stack: vec![0; 16],
        }
    }

    pub fn load_rom(&mut self, code: &[u8], rodata: &[u8]) {
        assert_eq!(self.cycles, 0);
        assert_eq!(self.regs.pc, 0);

        self.code[0..code.len()].copy_from_slice(code);
        self.rodata.copy_from_slice(rodata);
    }

    pub fn load_rom_combined(&mut self, rom: &[u8]) {
        assert_eq!(rom.len(), 8192);
        self.load_rom(&rom[0..6144], &rom[6144..]);
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
        let pc = self.regs.pc as usize;
        Instruction::decode(&mut self.code[pc..].iter().copied())
    }

    /// Fetches and decodes the next instruction at PC
    pub fn fetch_next_instr(&mut self) -> Result<Instruction> {
        let pc = self.regs.pc as usize;
        Instruction::decode(&mut self.code[pc..].iter().copied())
    }

    fn push(&mut self, val: u16) {
        let sp = self.regs.read(Register::SP);
        self.stack[sp as usize] = val;
        self.regs.write(Register::SP, sp + 1);
    }

    /// Executes one CPU step (one instruction).
    pub fn step(&mut self) -> Result<Ticks> {
        println!("{}", self.dump_state());

        let start_cycles = self.cycles;
        let instr = self.fetch_next_instr()?;

        self.regs
            .write(Register::PC, self.regs.pc.wrapping_add(instr.len() as u16));
        self.execute_instruction(&instr)?;

        Ok(self.cycles - start_cycles)
    }

    /// Executes an instruction.
    fn execute_instruction(&mut self, instr: &Instruction) -> Result<()> {
        match instr {
            Instruction::Jp(i) if i.branch_cond() == BrchCnd::CALL => {
                // CALL
                self.push(self.regs.read(Register::PC));
                self.regs.write(Register::PC, i.next_address());
                Ok(())
            }
            Instruction::Jp(i) => self.op_jp(i),
            _ => todo!(),
        }
    }

    /// JP*
    fn op_jp(&mut self, instr: &InstructionJp) -> Result<()> {
        let cond = match instr.branch_cond() {
            BrchCnd::JMP => true,
            BrchCnd::CALL => unreachable!(),

            BrchCnd::JNCA => !self.regs.test_flag(Flags::A, Flag::C),
            BrchCnd::JCA => self.regs.test_flag(Flags::A, Flag::C),
            BrchCnd::JNCB => !self.regs.test_flag(Flags::B, Flag::C),
            BrchCnd::JCB => self.regs.test_flag(Flags::B, Flag::C),
            BrchCnd::JNZA => !self.regs.test_flag(Flags::A, Flag::Z),
            BrchCnd::JZA => self.regs.test_flag(Flags::A, Flag::Z),
            BrchCnd::JNZB => !self.regs.test_flag(Flags::B, Flag::Z),
            BrchCnd::JZB => self.regs.test_flag(Flags::B, Flag::Z),
            BrchCnd::JNOVA0 => !self.regs.test_flag(Flags::A, Flag::OV0),
            BrchCnd::JOVA0 => self.regs.test_flag(Flags::A, Flag::OV0),
            BrchCnd::JNOVB0 => !self.regs.test_flag(Flags::B, Flag::OV0),
            BrchCnd::JOVB0 => self.regs.test_flag(Flags::B, Flag::OV0),
            BrchCnd::JNOVA1 => !self.regs.test_flag(Flags::A, Flag::OV1),
            BrchCnd::JOVA1 => self.regs.test_flag(Flags::A, Flag::OV1),
            BrchCnd::JNOVB1 => !self.regs.test_flag(Flags::B, Flag::OV1),
            BrchCnd::JOVB1 => self.regs.test_flag(Flags::B, Flag::OV1),
            BrchCnd::JNSA0 => !self.regs.test_flag(Flags::A, Flag::S0),
            BrchCnd::JSA0 => self.regs.test_flag(Flags::A, Flag::S0),
            BrchCnd::JNSB0 => !self.regs.test_flag(Flags::B, Flag::S0),
            BrchCnd::JSB0 => self.regs.test_flag(Flags::B, Flag::S0),
            BrchCnd::JNSA1 => !self.regs.test_flag(Flags::A, Flag::S1),
            BrchCnd::JSA1 => self.regs.test_flag(Flags::A, Flag::S1),
            BrchCnd::JNSB1 => !self.regs.test_flag(Flags::B, Flag::S1),
            BrchCnd::JSB1 => self.regs.test_flag(Flags::B, Flag::S1),
            BrchCnd::JDPL0 => self.regs.read(Register::DP) & 0x0F == 0,
            BrchCnd::JDPLN0 => self.regs.read(Register::DP) & 0x0F != 0,
            BrchCnd::JDPLF => self.regs.read(Register::DP) & 0x0F == 0x0F,
            BrchCnd::JDPLNF => self.regs.read(Register::DP) & 0x0F != 0x0F,

            // Serial stuff, not used on SNES
            BrchCnd::JNSIAK => todo!(),
            BrchCnd::JSIAK => todo!(),
            BrchCnd::JNSOAK => todo!(),
            BrchCnd::JSOAK => todo!(),

            BrchCnd::JNRQM => !self.regs.test_sr(SR::RQM),
            BrchCnd::JRQM => self.regs.test_sr(SR::RQM),
        };

        if cond {
            // Branch taken
            self.regs.write(Register::PC, instr.next_address());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_traits::ToPrimitive;

    fn cpu(op: u32) -> CpuUpd77c25 {
        let mut c = CpuUpd77c25::new();
        c.code[2] = (op >> 16) as u8;
        c.code[1] = (op >> 8) as u8;
        c.code[0] = op as u8;
        c
    }

    fn cpu_run(op: u32) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.step().unwrap();
        c
    }

    fn cpu_run_sr(op: u32, sr: SR) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.regs.sr = 1 << sr.to_u16().unwrap();
        c.step().unwrap();
        c
    }

    fn cpu_run_reg(op: u32, reg: Register, val: u16) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.regs.write(reg, val);
        c.step().unwrap();
        c
    }

    fn cpu_run_flag(op: u32, flags: Flags, flag: Flag) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.regs.flags[flags.to_usize().unwrap()] = 1 << flag.to_u16().unwrap();
        println!("{}", c.dump_state());
        c.step().unwrap();
        println!("{}", c.dump_state());
        c
    }

    #[test]
    fn jp_flags() {
        let test = |flg, opcode: u32| {
            println!("{:?} {:06X}", flg, opcode);

            // Flags A
            // D.14 indicates true/false
            // D.15 indicates A/B
            let opcode = opcode & !(1 << 15);
            let nopcode = opcode & !(1 << 14);
            let yopcode = opcode | (1 << 14);
            assert_eq!(cpu_run(nopcode).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(nopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(nopcode, Flags::B, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run(yopcode).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(yopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(yopcode, Flags::B, flg).regs.pc, 0x0000);

            // Flags B
            let opcode = opcode | (1 << 15);
            let nopcode = opcode & !(1 << 14);
            let yopcode = opcode | (1 << 14);
            assert_eq!(cpu_run(nopcode).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(nopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(nopcode, Flags::B, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run(yopcode).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(yopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(yopcode, Flags::B, flg).regs.pc, 0x0000);
        };
        test(Flag::C, 0b10_010000000_00000000000_00);
        test(Flag::Z, 0b10_010001000_00000000000_00);
        test(Flag::OV0, 0b10_010010000_00000000000_00);
        test(Flag::OV1, 0b10_010011000_00000000000_00);
        test(Flag::S0, 0b10_010100000_00000000000_00);
        test(Flag::S1, 0b10_010101000_00000000000_00);
    }

    #[test]
    fn jp_sr_rqm() {
        assert_ne!(cpu_run(0x97C000).regs.pc, 0x0000);
        assert_eq!(cpu_run_sr(0x97C000, SR::RQM).regs.pc, 0x0000);
    }

    #[test]
    fn jp_jdpl0() {
        assert_eq!(
            cpu_run_reg(0b10_010110000_00000000000_00, Register::DP, 0)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110000_00000000000_00, Register::DP, 0x0010)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110000_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdpln0() {
        assert_ne!(
            cpu_run_reg(0b10_010110001_00000000000_00, Register::DP, 0)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110001_00000000000_00, Register::DP, 0x0010)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110001_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdplnf() {
        assert_ne!(
            cpu_run_reg(0b10_010110011_00000000000_00, Register::DP, 0x000F)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110011_00000000000_00, Register::DP, 0x001F)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110011_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdplf() {
        assert_eq!(
            cpu_run_reg(0b10_010110010_00000000000_00, Register::DP, 0x000F)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110010_00000000000_00, Register::DP, 0x001F)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110010_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jmp() {
        assert_eq!(cpu_run(0b10_100000000_00000000000_00).regs.pc, 0x0000);
        assert_eq!(cpu_run(0b10_100000000_00000000000_00).regs.sp, 0);
        assert_eq!(cpu_run(0b10_100000000_00000001000_00).regs.pc, 0x0008);
    }

    #[test]
    fn call() {
        let c = cpu_run(0b10_101000000_00000001000_00);
        assert_eq!(c.regs.pc, 0x0008);
        assert_eq!(c.regs.sp, 0x0001);
        assert_eq!(c.stack[0], 0x0003);
    }
}
