mod bus;

use std::cell::RefCell;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, Bus, BusMember};
use crate::cpu_65816::cpu::Cpu65816;
use crate::tickable::{Tickable, Ticks};

use bus::*;

const IRAM_SIZE: usize = 2 * 1024;
const BWRAM_SIZE: usize = 256 * 1024;

/// SA-1 co-processor
#[derive(Serialize, Deserialize)]
pub struct SA1 {
    /// CPU core
    pub cpu: RefCell<Cpu65816<Sa1Bus>>,

    /// SA-1 CPU control shadow register
    pub ccnt: u8,
}

impl SA1 {
    pub fn new(rom: &[u8], rom_mask: usize) -> Self {
        let mut cpu = Cpu65816::new(Sa1Bus::new(rom.to_owned(), rom_mask));

        // Address of CRV
        cpu.vec_reset = 0x2203;
        // Address of CNV
        cpu.intvec_nmi = 0x2205;
        // Address of CIV
        cpu.intvec_int = 0x2207;

        Self {
            cpu: RefCell::new(cpu),
            ccnt: CCNT_DEFAULT,
        }
    }

    /// Interrupt line for S-CPU
    pub fn get_int(&self) -> bool {
        let cpu = self.cpu.borrow();
        let v = cpu.bus.snes_irq && cpu.bus.sie & SIE_IRQ != 0;
        if v {
            println!("SNES IRQ!");
        }
        v
    }
}

impl Tickable for SA1 {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        let mut cpu = self.cpu.borrow_mut();

        if self.ccnt != cpu.bus.ccnt {
            if self.ccnt & CCNT_RESET != 0 && cpu.bus.ccnt & CCNT_RESET == 0 {
                cpu.reset();
                println!("SA-1 go @ {:04X}", cpu.regs.pc);
            }

            self.ccnt = cpu.bus.ccnt;
        }

        // Run CPU if not in WAIT or RESET
        if self.ccnt & (CCNT_WAIT | CCNT_RESET) == 0 {
            cpu.tick(ticks)
        } else {
            Ok(0)
        }
    }
}

impl BusMember<Address> for SA1 {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        let cpu = self.cpu.borrow();

        // Note: SA-1 has a flexible mapper, so any cartridge access is forwarded here.
        // For the SNES-side, we filter everything that is SA-1 only and forward
        // the rest to the SA-1's bus.
        match (bank, addr) {
            // I/O ports
            (0x00..=0x3F | 0x80..=0xBF, 0x2200..=0x23FF) |
            // I-RAM (not re-mappable)
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x37FF) |
            // BW-RAM (mappable 8K block)
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) |
            // LoROM (mappable)
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) |
            // BW-RAM (not re-mappable)
            (0x40..=0x4F, _) |
            // HiROM
            (0xC0..=0xFF, _) => Some(cpu.bus.read(fulladdr)),

            // Handled by SNES
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        let mut cpu = self.cpu.borrow_mut();

        // Note: SA-1 has a flexible mapper, so any cartridge access is forwarded here.
        // For the SNES-side, we filter everything that is SA-1 only and forward
        // the rest to the SA-1's bus.
        match (bank, addr) {
            // I/O ports
            (0x00..=0x3F | 0x80..=0xBF, 0x2200..=0x23FF) |
            // I-RAM (not re-mappable)
            (0x00..=0x3F | 0x80..=0xBF, 0x3000..=0x37FF) |
            // BW-RAM (mappable 8K block)
            (0x00..=0x3F | 0x80..=0xBF, 0x6000..=0x7FFF) |
            // LoROM (mappable)
            (0x00..=0x3F | 0x80..=0xBF, 0x8000..=0xFFFF) |
            // BW-RAM (not re-mappable)
            (0x40..=0x4F, _) |
            // HiROM
            (0xC0..=0xFF, _) => Some(cpu.bus.write(fulladdr, val)),

            // Handled by SNES
            _ => None,
        }
    }
}
