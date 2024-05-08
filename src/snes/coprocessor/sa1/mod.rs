mod bus;

use std::cell::RefCell;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, Bus, BusMember};
use crate::cpu_65816::cpu::Cpu65816;
use crate::tickable::{Tickable, Ticks};

use bus::Sa1Bus;

const IRAM_SIZE: usize = 2 * 1024;
const BWRAM_SIZE: usize = 256 * 1024;

/// SA-1 co-processor
#[derive(Serialize, Deserialize)]
pub struct SA1 {
    /// CPU core
    pub cpu: RefCell<Cpu65816<Sa1Bus>>,

    /// SA-1 CPU control shadow register
    pub cpu_ctrl: u8,
}

impl SA1 {
    pub fn new(rom: &[u8], rom_mask: usize) -> Self {
        Self {
            cpu: RefCell::new(Cpu65816::new(Sa1Bus::new(rom.to_owned(), rom_mask))),
            cpu_ctrl: 0x20,
        }
    }
}

impl Tickable for SA1 {
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        let mut cpu = self.cpu.borrow_mut();

        // ??? do these have the extra indirection?
        cpu.vec_reset = cpu.bus.sa1_crv;
        cpu.intvec_nmi = cpu.bus.sa1_cnv;
        cpu.intvec_int = cpu.bus.sa1_civ;

        if self.cpu_ctrl != cpu.bus.sa1_cpu_ctrl {
            if self.cpu_ctrl & 0x20 != 0 && cpu.bus.sa1_cpu_ctrl & 0x20 == 0 {
                let pc = cpu.bus.sa1_crv as u16;
                cpu.reset_pc(pc);
                println!("SA-1 go @ {:04X}", cpu.regs.pc);
            }

            self.cpu_ctrl = cpu.bus.sa1_cpu_ctrl;
        }

        if self.cpu_ctrl & 0x20 == 0 {
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
