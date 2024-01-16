use std::cell::{Cell, RefCell};

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::snes::bus::{Address, BusMember};
use crate::snes::cpu_upd77c25::cpu::CpuUpd77c25;
use crate::snes::cpu_upd77c25::regs::{Register, SR};
use crate::tickable::{Tickable, Ticks};

/// DSP-1 co-processor
#[derive(Serialize, Deserialize)]
pub struct DSP1 {
    cpu: RefCell<CpuUpd77c25>,

    /// Flip-flop to switch reading LSB/MSB of SR
    sr_read_msb: Cell<bool>,
}

impl DSP1 {
    pub fn new() -> Self {
        Self {
            cpu: RefCell::new(CpuUpd77c25::new()),
            sr_read_msb: Cell::new(false),
        }
    }

    pub fn load_rom_combined(&mut self, rom: &[u8]) {
        let mut cpu = self.cpu.borrow_mut();
        cpu.load_rom_combined(rom)
    }
}

impl BusMember<Address> for DSP1 {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x6000..=0x6FFF => {
                // DR
                let mut cpu = self.cpu.borrow_mut();
                let dr = cpu.regs.read(Register::DR);
                if !cpu.regs.test_sr(SR::DRC) {
                    // DR in 16-bit
                    if !cpu.regs.test_sr(SR::DRS) {
                        // LSB
                        cpu.regs.write_sr(&[(SR::DRS, true)]);
                        Some(dr as u8)
                    } else {
                        // MSB
                        cpu.regs.write_sr(&[(SR::RQM, false), (SR::DRS, false)]);
                        Some((dr >> 8) as u8)
                    }
                } else {
                    // 8-bit
                    cpu.regs.write_sr(&[(SR::RQM, false)]);
                    Some(dr as u8)
                }
            }
            0x7000..=0x7FFF => {
                // SR
                let cpu = self.cpu.borrow();
                let msb = self.sr_read_msb.get();
                self.sr_read_msb.set(!msb);
                if msb {
                    Some((cpu.regs.read(Register::SR) >> 8) as u8)
                } else {
                    Some(cpu.regs.read(Register::SR) as u8)
                }
            }
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            0x6000..=0x6FFF => {
                // DR
                let mut cpu = self.cpu.borrow_mut();
                let dr = cpu.regs.read(Register::DR);
                if !cpu.regs.test_sr(SR::DRC) {
                    // DR in 16-bit
                    if !cpu.regs.test_sr(SR::DRS) {
                        // LSB
                        cpu.regs.write_sr(&[(SR::DRS, true)]);
                        cpu.regs.write(Register::DR, (dr & 0xFF00) | val as u16);
                        Some(())
                    } else {
                        // MSB
                        cpu.regs.write_sr(&[(SR::RQM, false), (SR::DRS, false)]);
                        cpu.regs
                            .write(Register::DR, (dr & 0x00FF) | ((val as u16) << 8));
                        Some(())
                    }
                } else {
                    // 8-bit
                    cpu.regs.write_sr(&[(SR::RQM, false)]);
                    Some(cpu.regs.write(Register::DR, val as u16))
                }
            }
            _ => None,
        }
    }
}

impl Tickable for DSP1 {
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        // TODO more granular or only as much as needed based on comms?
        let mut cpu = self.cpu.borrow_mut();
        cpu.step()?;
        Ok(())
    }
}
