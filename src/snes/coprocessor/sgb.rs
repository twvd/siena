use std::cell::Cell;
use std::fs;

use anyhow::{Context, Result};
use crossbeam_channel::Receiver;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::bus::{Address, BusMember};
use crate::cpu_sm83::cpu::CpuSm83;
use crate::gameboy::bus::gbbus::Gameboybus;
use crate::gameboy::cartridge::cartridge;
use crate::gameboy::joypad::Button as GbButton;
use crate::gameboy::lcd::Color as GbColor;
use crate::gameboy::lcd::LCDController;
use crate::tickable::{Tickable, Ticks};

/// Joypad ICD2 bit mask
/// Bit 4 indicates a 0, bit 5 indicates a 1.
/// Both bits low indicates a reset.
const JOYP_MASK: u8 = 0x30;

/// Amount of scanlines in one display buffer row
const DISPLAY_ROW_HEIGHT: usize = 8;

/// Amount of display buffers
const DISPLAY_BUFFERS: usize = 4;

/// Size of a single display buffer
const DISPLAY_BUFFER_SIZE: usize = 320;

/// Scanline on which Gameboy VBlank starts
const GB_VBLANK_START: usize = 144;

/// BPP of the Super Gameboy display
const DISPLAY_BPP: usize = 2;

#[derive(FromPrimitive)]
enum SGBDivider {
    /// 5 MHz
    Div4 = 0,
    /// 4 MHz
    Div5 = 1,
    /// 3 MHz
    Div7 = 2,
    /// 2.3 MHz
    Div9 = 3,
}

impl SGBDivider {
    pub fn to_div(&self) -> Ticks {
        match self {
            SGBDivider::Div4 => 4,
            SGBDivider::Div5 => 5,
            SGBDivider::Div7 => 7,
            SGBDivider::Div9 => 9,
        }
    }
}

/// Super Gameboy co-processor
pub struct SuperGameboy {
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

    scanline_recv: Receiver<(usize, Vec<GbColor>)>,

    /// The rotating character buffers
    display_buffers: [[u8; DISPLAY_BUFFER_SIZE]; DISPLAY_BUFFERS],

    /// Current display buffer being written
    display_buffer_w: usize,

    /// Current display buffer selected for read
    display_buffer_r: usize,

    /// Current read byte position
    display_buffer_r_byte: Cell<usize>,

    /// Current display row (0x11 = VBlank)
    display_row: u8,

    /// SNES master clock divider
    clockdiv: SGBDivider,
}

impl SuperGameboy {
    pub fn new(rom_game: &[u8]) -> Result<Self> {
        let rom_boot = fs::read("sgb_boot.bin")
            .with_context(|| "SGB Gameboy boot ROM (sgb_boot.bin) not found")?;

        let cart = cartridge::load(&rom_game);
        println!("Loaded Gameboy cartridge: {}", cart);

        let mut lcd = LCDController::new(false);
        let scanline_recv = lcd.get_scanline_output();
        let bus = Gameboybus::new(cart, Some(&rom_boot), lcd, false);
        let cpu = CpuSm83::new(bus, false);

        Ok(Self {
            ticks: 0,
            cpu,
            joyp_last: 0,
            data_byte_in: 0,
            data_byte_in_b: 0,
            data_bytes: usize::MAX,
            data_packet: [0; 16],
            data_packet_ready: Cell::new(false),
            run: false,
            scanline_recv,

            display_buffers: [[0; DISPLAY_BUFFER_SIZE]; DISPLAY_BUFFERS],
            display_buffer_r: 0,
            display_buffer_r_byte: Cell::new(0),
            display_buffer_w: 0,
            display_row: 0,
            rom: rom_game.to_vec(),
            clockdiv: SGBDivider::Div5,
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

    pub fn poll_display(&mut self) -> Result<()> {
        while let Ok((scanline, colors)) = self.scanline_recv.try_recv() {
            if scanline >= GB_VBLANK_START {
                // In VBlank
                if self.display_row != 0x11 {
                    // Just entered VBlank
                    self.display_row = 0x11;
                    self.display_buffer_w = (self.display_buffer_w + 1) % DISPLAY_BUFFERS;
                }

                continue;
            }

            self.display_row = (scanline / DISPLAY_ROW_HEIGHT) as u8;
            if scanline % DISPLAY_ROW_HEIGHT == 0 {
                // Next buffer
                self.display_buffer_w = (self.display_buffer_w + 1) % DISPLAY_BUFFERS;
                self.display_buffers[self.display_buffer_w] = [0; DISPLAY_BUFFER_SIZE];
                if self.display_buffer_w == self.display_buffer_r {
                    println!("SNES running too slow! Scanline {}", scanline);
                }
            }

            // Convert the array of pixels to SNES 8x8 tiles
            let voffset = (scanline % DISPLAY_ROW_HEIGHT) * DISPLAY_BPP;
            for (x, c) in colors.into_iter().enumerate() {
                let poffset = voffset + (x / 8) * (DISPLAY_BPP * 8);
                let bit = 1 << (7 - (x % 8));

                for bitplane in 0..DISPLAY_BPP {
                    if c & (1 << bitplane) != 0 {
                        self.display_buffers[self.display_buffer_w][poffset + bitplane] |= bit;
                    }
                }
            }
        }
        Ok(())
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
        self.poll_display()?;

        Ok(step_ticks * self.clockdiv.to_div())
    }
}

impl BusMember<Address> for SuperGameboy {
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            // LCD Character Row and Buffer Write-Row
            0x6000 => Some(((self.display_buffer_w as u8) & 3) | (self.display_row << 3)),
            // Packet available flag
            0x6002 if self.data_packet_ready.get() => Some(1),
            0x6002 => Some(0),

            // Data packet buffer
            0x7000 => {
                self.data_packet_ready.set(false);
                Some(self.data_packet[0])
            }
            0x7001..=0x700F => Some(self.data_packet[addr - 0x7000]),

            // Character buffer data
            0x7800 => {
                let val = if self.display_buffer_r_byte.get() >= 320 {
                    0xFF
                } else {
                    self.display_buffers[self.display_buffer_r][self.display_buffer_r_byte.get()]
                };
                self.display_buffer_r_byte
                    .set((self.display_buffer_r_byte.get() + 1) % 512);
                Some(val)
            }
            _ => None,
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);
        match addr {
            // Character Buffer Read Row Select
            0x6001 => {
                self.display_buffer_r = (val & 3) as usize;
                self.display_buffer_r_byte.set(0);
                Some(())
            }
            // Reset/Multiplayer/Speed Control (W)
            0x6003 => {
                if self.run != (val & 0x80 != 0) {
                    self.reset_gameboy();
                    self.run = val & 0x80 != 0;
                    println!("Gameboy run: {}", self.run);
                }
                self.clockdiv = SGBDivider::from_u8(val & 3).unwrap();
                Some(())
            }
            // Controller data - joypad 1
            0x6004 => {
                self.cpu
                    .bus
                    .joypad
                    .set(GbButton::DPadRight, val & 0x01 == 0);
                self.cpu.bus.joypad.set(GbButton::DPadLeft, val & 0x02 == 0);
                self.cpu.bus.joypad.set(GbButton::DPadUp, val & 0x04 == 0);
                self.cpu.bus.joypad.set(GbButton::DPadDown, val & 0x08 == 0);
                self.cpu.bus.joypad.set(GbButton::A, val & 0x10 == 0);
                self.cpu.bus.joypad.set(GbButton::B, val & 0x20 == 0);
                self.cpu.bus.joypad.set(GbButton::Select, val & 0x40 == 0);
                self.cpu.bus.joypad.set(GbButton::Start, val & 0x80 == 0);
                Some(())
            }
            _ => None,
        }
    }
}
