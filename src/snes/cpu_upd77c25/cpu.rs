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
    pub rom: Vec<u8>,
    pub ram: Vec<u8>,
    pub stack: Vec<u8>,
}

impl CpuUpd77c25 {
    pub fn new(rom: &[u8]) -> Self {
        Self {
            regs: RegisterFile::from_pc(0),
            cycles: 0,
            rom: Vec::from(rom),
            ram: vec![0; 256],
            stack: vec![0; 16],
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
        let pc = self.regs.pc as usize;
        Instruction::decode(&mut self.rom[pc..].iter().copied())
    }

    /// Fetches and decodes the next instruction at PC
    pub fn fetch_next_instr(&mut self) -> Result<Instruction> {
        let pc = self.regs.pc as usize;
        Instruction::decode(&mut self.rom[pc..].iter().copied())
    }

    /// Executes one CPU step (one instruction).
    pub fn step(&mut self) -> Result<Ticks> {
        let start_cycles = self.cycles;
        let instr = self.fetch_next_instr()?;

        self.regs.write(Register::PC, self.regs.pc.wrapping_add(instr.len() as u16));
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
