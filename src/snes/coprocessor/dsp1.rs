use std::cell::{Cell, RefCell};

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::snes::cpu_upd77c25::cpu::CpuUpd77c25;
use crate::snes::cpu_upd77c25::regs::{Register, SR};
use crate::tickable::{Tickable, Ticks};

/// DSP-1 co-processor
#[derive(Serialize, Deserialize)]
pub struct DSP1 {
    /// uPD77C25 CPU core
    cpu: RefCell<CpuUpd77c25>,

    /// Last seen CPU PC register (for busy loop detection)
    last_pc: u16,

    /// Flip-flop to switch reading LSB/MSB of SR
    sr_read_msb: Cell<bool>,
}

impl DSP1 {
    pub fn new() -> Self {
        Self {
            last_pc: 0,
            cpu: RefCell::new(CpuUpd77c25::new()),
            sr_read_msb: Cell::new(false),
        }
    }

    pub fn load_rom_combined(&mut self, rom: &[u8]) {
        let mut cpu = self.cpu.borrow_mut();
        cpu.load_rom_combined(rom)
    }

    pub fn read_dr(&self) -> u8 {
        let mut cpu = self.cpu.borrow_mut();
        let dr = cpu.regs.read(Register::DR);
        if !cpu.regs.test_sr(SR::DRC) {
            // DR in 16-bit
            if !cpu.regs.test_sr(SR::DRS) {
                // LSB
                cpu.regs.write_sr(&[(SR::DRS, true)]);
                dr as u8
            } else {
                // MSB
                cpu.regs.write_sr(&[(SR::RQM, false), (SR::DRS, false)]);
                (dr >> 8) as u8
            }
        } else {
            // 8-bit
            cpu.regs.write_sr(&[(SR::RQM, false)]);
            dr as u8
        }
    }

    pub fn read_sr(&self) -> u8 {
        let cpu = self.cpu.borrow();
        let msb = self.sr_read_msb.get();
        self.sr_read_msb.set(!msb);
        if msb {
            (cpu.regs.read(Register::SR) >> 8) as u8
        } else {
            cpu.regs.read(Register::SR) as u8
        }
    }

    pub fn write_dr(&mut self, val: u8) {
        let mut cpu = self.cpu.borrow_mut();
        let dr = cpu.regs.read(Register::DR);
        if !cpu.regs.test_sr(SR::DRC) {
            // DR in 16-bit
            if !cpu.regs.test_sr(SR::DRS) {
                // LSB
                cpu.regs.write_sr(&[(SR::DRS, true)]);
                cpu.regs.write(Register::DR, (dr & 0xFF00) | val as u16);
            } else {
                // MSB
                cpu.regs.write_sr(&[(SR::RQM, false), (SR::DRS, false)]);
                cpu.regs
                    .write(Register::DR, (dr & 0x00FF) | ((val as u16) << 8));
            }
        } else {
            // 8-bit
            cpu.regs.write_sr(&[(SR::RQM, false)]);
            cpu.regs.write(Register::DR, val as u16);
        }
    }
}

impl Tickable for DSP1 {
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        let mut cpu = self.cpu.borrow_mut();

        // Detect busy loops (JRQM $PC)
        if !cpu.regs.test_sr(SR::RQM) || (cpu.regs.pc != self.last_pc) {
            self.last_pc = cpu.regs.pc;
            cpu.step()?;
        }

        Ok(())
    }
}
