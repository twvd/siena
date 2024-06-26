use std::cell::RefCell;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, BusMember};
use crate::cpu_gsu::cpu::{CpuGsu, GsuAddress, GsuMap, CACHE_LINE_SIZE};
use crate::cpu_gsu::regs::{Flag, Register};
use crate::tickable::{Tickable, Ticks};

/// SuperFX co-processor
#[derive(Serialize, Deserialize)]
pub struct SuperFX {
    /// GSU CPU core
    pub cpu: RefCell<CpuGsu>,
}

impl SuperFX {
    pub fn new(rom: &[u8], map: GsuMap, ram_mask: usize) -> Self {
        Self {
            cpu: RefCell::new(CpuGsu::new(rom, map, ram_mask)),
        }
    }

    pub fn get_int(&mut self) -> bool {
        let mut cpu = self.cpu.borrow_mut();
        cpu.get_int()
    }
}

impl Tickable for SuperFX {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        let mut cpu = self.cpu.borrow_mut();

        cpu.tick(ticks)
    }
}

impl BusMember<Address> for SuperFX {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        let mut cpu = self.cpu.borrow_mut();

        match addr {
            0x3000..=0x301F => {
                // Rxx registers
                let r = (addr & 0x1F) >> 1;
                if addr & 1 == 0 {
                    // LSB
                    Some(cpu.regs.read_r(r) as u8)
                } else {
                    // MSB
                    Some((cpu.regs.read_r(r) >> 8) as u8)
                }
            }
            0x3030 => {
                let v = cpu.regs.read(Register::SFR) as u8;
                Some(v)
            }
            0x3031 => {
                let v = cpu.regs.read(Register::SFR);
                cpu.regs.write_flags(&[(Flag::IRQ, false)]);
                Some((v >> 8) as u8)
            }
            // 0x3032 unused
            0x3033 => Some(cpu.regs.read8(Register::BRAMBR)),
            0x3034 => Some(cpu.regs.read8(Register::PBR)),
            // 0x3035 unused
            0x3036 => Some(cpu.regs.read8(Register::ROMBR)),
            0x3037 => Some(cpu.regs.read8(Register::CFGR)),
            0x3038 => Some(cpu.regs.read8(Register::SCBR)),
            0x3039 => Some(cpu.regs.read8(Register::CLSR)),
            0x303A => Some(cpu.regs.read8(Register::SCMR)),
            0x303B => Some(cpu.regs.read8(Register::VCR)),
            0x303C => Some(cpu.regs.read8(Register::RAMBR)),
            // 0x303D unused
            0x303E => Some(cpu.regs.read(Register::CBR) as u8),
            0x303F => Some((cpu.regs.read(Register::CBR) >> 8) as u8),

            // Instruction cache
            0x3100..=0x32FF => {
                let base = cpu.get_cache_base() & 0x1FF;
                Some(cpu.read_bus(base + ((addr as GsuAddress) - 0x3100)))
            }

            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        let mut cpu = self.cpu.borrow_mut();

        match addr {
            0x3000..=0x301D => {
                // Rxx registers
                let r = (addr & 0x1F) >> 1;
                let curval = cpu.regs.read_r(r);
                let newval = if addr & 1 == 0 {
                    // LSB
                    (curval & 0xFF00) | (val as u16)
                } else {
                    // MSB
                    (curval & 0xFF) | ((val as u16) << 8)
                };
                cpu.regs.write_r(r, newval);
                Some(())
            }
            0x301E..=0x301F => {
                // R15 register
                let curval = cpu.regs.get_r15();
                let newval = if addr & 1 == 0 {
                    // LSB
                    (curval & 0xFF00) | (val as u16)
                } else {
                    // MSB
                    (curval & 0xFF) | ((val as u16) << 8)
                };
                cpu.regs.set_r15(newval);

                // If PC (R15) is written, start execution
                if addr & 1 != 0 && !cpu.regs.test_flag(Flag::G) {
                    cpu.regs.get_clr_r15_shadow();
                    cpu.regs.write_flags(&[(Flag::G, true)]);
                }

                Some(())
            }
            0x3030 => {
                let curval = cpu.regs.read(Register::SFR);
                Some(
                    cpu.regs
                        .write(Register::SFR, (curval & 0xFF00) | (val as u16)),
                )
            }
            0x3031 => {
                let curval = cpu.regs.read(Register::SFR);
                let newval = (curval & 0xFF) | ((val as u16) << 8);

                cpu.regs.write(Register::SFR, newval);

                // Writing G to 0 invalidates cache and resets CBR
                if !cpu.regs.test_flag(Flag::G) {
                    cpu.regs.write(Register::CBR, 0);
                    cpu.cache_flush();
                }
                Some(())
            }
            // 0x3032 unused
            0x3033 => Some(cpu.regs.write8(Register::BRAMBR, val)),
            0x3034 => Some(cpu.regs.write8(Register::PBR, val)),
            // 0x3035 unused
            0x3036 => Some(cpu.regs.write8(Register::ROMBR, val)),
            0x3037 => Some(cpu.regs.write8(Register::CFGR, val)),
            0x3038 => Some(cpu.regs.write8(Register::SCBR, val)),
            0x3039 => Some(cpu.regs.write8(Register::CLSR, val)),
            0x303A => Some(cpu.regs.write8(Register::SCMR, val)),
            0x303B => Some(cpu.regs.write8(Register::VCR, val)),
            0x303C => Some(cpu.regs.write8(Register::RAMBR, val)),
            // 0x303D unused
            // 0x303E CBR read-only
            // 0x303F CBR read-only

            // Instruction cache
            0x3100..=0x32FF => {
                let cache_addr = addr - 0x3100;
                cpu.cache[cache_addr] = val;
                if cache_addr % CACHE_LINE_SIZE == (CACHE_LINE_SIZE - 1) {
                    cpu.cache_valid[cache_addr / CACHE_LINE_SIZE] = true;
                }
                Some(())
            }

            _ => None,
        }
    }
}
