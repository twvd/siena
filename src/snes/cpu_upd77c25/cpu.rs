use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::tickable::Ticks;

use super::instruction::Instruction;
use super::regs::{Flag, Register, RegisterFile};

/// NEC uPD77C25
#[derive(Serialize, Deserialize)]
pub struct CpuUpd77c25 {
    pub regs: RegisterFile,
    pub cycles: Ticks,
    pub rodata: Vec<u8>,
    pub code: Vec<u8>,
    pub ram: Vec<u8>,
    pub stack: Vec<u8>,
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

    /// Executes one CPU step (one instruction).
    pub fn step(&mut self) -> Result<Ticks> {
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
            _ => todo!(),
        }
    }
}
