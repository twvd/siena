use anyhow::Result;

use crate::frontend::sdl::SDLRenderer;
use crate::snes::bus::{Address, Bus, BusMember};
use crate::snes::ppu::PPU;
use crate::tickable::{Tickable, Ticks};

const WRAM_BANKS: usize = 2;
const WRAM_BANK_SIZE: usize = 64 * 1024;
const WRAM_SIZE: usize = WRAM_BANKS * WRAM_BANK_SIZE;

#[derive(Eq, PartialEq, Copy, Clone, clap::ValueEnum)]
pub enum BusTrace {
    None,
    Open,
    All,
}

/// All peripherals as they face the main CPU
pub struct Mainbus {
    cartridge: Vec<u8>,
    wram: Vec<u8>,
    trace: BusTrace,

    /// Picture Processing Unit
    ppu: PPU<SDLRenderer>,

    /// MEMSEL - Memory-2 Waitstate Control
    memsel: u8,
}

impl Mainbus {
    pub fn new(cartridge: &[u8], trace: BusTrace, ppu: PPU<SDLRenderer>) -> Self {
        Self {
            cartridge: cartridge.to_owned(),
            wram: vec![0; WRAM_SIZE],
            trace,

            ppu,

            memsel: 0,
        }
    }
}

impl Bus for Mainbus {
    fn read(&self, fulladdr: Address) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        let mapped_val = match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                // WRAM mirror
                0x0000..=0x1FFF => Some(self.wram[addr]),
                // Picture Processing Unit
                0x2100..=0x213F => self.ppu.read(fulladdr),
                // MEMSEL - Memory-2 Waitstate Control
                0x420D => Some(self.memsel),
                // RDNMI - V-Blank NMI Flag and CPU Version Number
                0x4210 => {
                    if self.ppu.in_vblank() {
                        Some(0x80 | 2)
                    } else {
                        Some(2)
                    }
                }
                // WS1 LoROM
                0x8000..=0xFFFF => Some(self.cartridge[addr - 0x8000 + bank * 0x8000]),

                _ => None,
            },
            // Full WRAM area
            0x7E..=0x7F => Some(self.wram[((bank - 0x7E) * WRAM_BANK_SIZE) + addr]),
            _ => None,
        };

        if let Some(v) = mapped_val {
            if self.trace == BusTrace::All {
                println!("Bus read: {:06X} = {:02X}", fulladdr, v);
            }
            v
        } else {
            // Open bus
            if self.trace != BusTrace::None {
                println!("Open/unimplemented bus read: {:06X}", fulladdr);
            }

            // TODO accurate open bus behaviour
            0xFF
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        let written = match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                // WRAM mirror
                0x0000..=0x1FFF => Some(self.wram[addr] = val),
                // Picture Processing Unit
                0x2100..=0x213F => self.ppu.write(fulladdr, val),
                // MEMSEL - Memory-2 Waitstate Control
                0x420D => Some(self.memsel = val),

                _ => None,
            },
            // Full WRAM area
            0x7E..=0x7F => Some(self.wram[((bank - 0x7E) * WRAM_BANK_SIZE) + addr] = val),

            _ => None,
        };

        if written.is_none() && self.trace != BusTrace::None {
            println!(
                "Open/unimplemented bus write: {:06X} = {:02X}",
                fulladdr, val
            );
        } else if self.trace == BusTrace::All {
            println!("Bus write: {:06X} = {:02X}", fulladdr, val);
        }
    }
}

impl Tickable for Mainbus {
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        self.ppu.tick(ticks)?;
        Ok(())
    }
}
