use std::cell::Cell;
use std::fs;

use anyhow::Result;

use crate::bus::{Address, BusMember};
use crate::cpu_sm83::cpu::CpuSm83;
use crate::gameboy::bus::gbbus::Gameboybus;
use crate::gameboy::cartridge::cartridge;
use crate::gameboy::lcd::LCDController;
use crate::tickable::{Tickable, Ticks};

/// Joypad ICD2 bit mask
/// Bit 4 indicates a 0, bit 5 indicates a 1.
/// Both bits low indicates a reset.
const JOYP_MASK: u8 = 0x30;

/// Super Gameboy co-processor
pub struct SuperGameboy {
    pub rownr: Cell<u16>,
    pub buffernr: Cell<u8>,

    rom: Vec<u8>,
    pub ticks: Ticks,
    pub cpu: CpuSm83<Gameboybus>,
    pub joyp_last: u8,
    pub data_byte_in: u8,
    pub data_byte_in_b: u8,
    pub data_bytes: usize,
    pub data_packet: [u8; 16],
    pub data_packet_ready: Cell<bool>,
    run: bool,
}

impl SuperGameboy {
    pub fn new() -> Result<Self> {
        let rom_game = fs::read("rom/test.gb")?;
        let rom_boot = fs::read("sgb_boot.bin")?;

        let cart = cartridge::load(&rom_game);
        println!("Loaded Gameboy cartridge: {}", cart);

        let lcd = LCDController::new(false);
        let bus = Gameboybus::new(cart, Some(&rom_boot), lcd, false);
        let cpu = CpuSm83::new(bus, false);

        Ok(Self {
            rownr: Cell::new(0x11 << 4),
            buffernr: Cell::new(0),
            ticks: 0,
            cpu,
            joyp_last: 0,
            data_byte_in: 0,
            data_byte_in_b: 0,
            data_bytes: usize::MAX,
            data_packet: [0; 16],
            data_packet_ready: Cell::new(false),
            run: false,
            rom: rom_game,
        })
    }

    pub fn reset_gameboy(&mut self) {
        let mut lcd = LCDController::new(false);
        let cart = cartridge::load(&self.rom);
        let scanline_recv = lcd.get_scanline_output();
        let bus = Gameboybus::new(cart, Some(&self.cpu.bus.boot_rom), lcd, false);
        let cpu = CpuSm83::new(bus, false);

        self.cpu = cpu;
        self.scanline_recv = scanline_recv;
    }

    pub fn poll_packets(&mut self) {
        // Check incoming data bits
        let joyp = self.cpu.bus.joypad.read() & JOYP_MASK;
        if self.joyp_last != joyp {
            self.joyp_last = joyp;
            if joyp == 0 {
                // Reset (bit 4 and 5 both low)
                self.data_byte_in = 0;
                self.data_bytes = 0;
                self.data_byte_in_b = 0;
                if self.data_packet_ready.get() {
                    // If SNES hasn't startee reading by now, the previous packet
                    // will be overwritten.
                    println!("SNES too late reading command packet!!!");
                }
            } else if self.data_bytes < 16 && joyp != JOYP_MASK {
                // Bit 4 = zero
                if joyp & (1 << 5) == 0 {
                    // Bit 5 = one
                    self.data_byte_in |= 1 << self.data_byte_in_b;
                }
                // Next bit
                self.data_byte_in_b += 1;

                if self.data_byte_in_b == 8 {
                    // Current byte complete
                    self.data_packet[self.data_bytes] = self.data_byte_in;

                    // Next byte
                    self.data_byte_in = 0;
                    self.data_byte_in_b = 0;
                    self.data_bytes += 1;

                    if self.data_bytes == 16 {
                        println!("New command packet: {:02X?}", self.data_packet);

                        self.data_packet_ready.set(true);
                    }
                }
            }
        }
    }
}

impl Tickable for SuperGameboy {
    fn tick(&mut self, _ticks: Ticks) -> Result<Ticks> {
        if !self.run {
            return Ok(0);
        }

        let step_ticks = self.cpu.step()?;
        self.ticks += step_ticks;

        self.poll_packets();

        Ok(step_ticks)
    }
}

impl BusMember<Address> for SuperGameboy {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            // LCD Character Row and Buffer Write-Row
            0x6000 => {
                self.rownr.set((self.rownr.get() + 1) % 0x120);
                if self.rownr.get() == 0 {
                    self.buffernr.set((self.buffernr.get() + 1) % 4);
                }
                println!("read row: {:X}", self.rownr.get() >> 4);
                Some((((self.rownr.get() >> 4) as u8) << 3) | (self.buffernr.get()))
            }
            // Packet available flag
            0x6002 if self.data_packet_ready.get() => Some(1),
            0x6002 => Some(0),

            // Data packet buffer
            0x7000 => {
                self.data_packet_ready.set(false);
                Some(self.data_packet[0])
            }
            0x7001..=0x700F => Some(self.data_packet[addr - 0x7000]),

            0x7800 => Some(0xAA),
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match addr {
            0x6003 => {
                if self.run != (val & 0x80 != 0) {
                    self.reset_gameboy();
                    self.run = val & 0x80 != 0;
                    println!("Gameboy run: {}", self.run);
                }
                Some(())
            }
            _ => None,
        }
    }
}
