use std::cell::{Cell, RefCell};

use anyhow::Result;
use dbg_hex::dbg_hex;

use crate::frontend::Renderer;
use crate::snes::bus::{Address, Bus, BusMember};
use crate::snes::joypad::{Joypad, JOYPAD_COUNT};
use crate::snes::ppu::PPU;
use crate::tickable::{Tickable, Ticks};

const WRAM_BANKS: usize = 2;
const WRAM_BANK_SIZE: usize = 64 * 1024;
const WRAM_SIZE: usize = WRAM_BANKS * WRAM_BANK_SIZE;
const WRAM_MASK: Address = (WRAM_SIZE - 1) as Address;

const DMA_CHANNELS: usize = 8;

#[derive(Eq, PartialEq, Copy, Clone, clap::ValueEnum)]
pub enum BusTrace {
    None,
    Open,
    All,
}

/// All peripherals as they face the main CPU
pub struct Mainbus<TRenderer>
where
    TRenderer: Renderer,
{
    cartridge: Vec<u8>,
    wram: Vec<u8>,
    pub trace: BusTrace,

    /// Controllers
    joypads: [Joypad; JOYPAD_COUNT],

    /// Picture Processing Unit
    pub ppu: PPU<TRenderer>,

    /// MEMSEL - Memory-2 Waitstate Control
    memsel: u8,

    /// DMA channels
    dma: [DMAChannel; DMA_CHANNELS],

    /// Enabled HDMA channels
    hdmaen: u8,

    /// WMADD - WRAM B-bus access port
    wmadd: Cell<Address>,

    /// Last read data for open bus emulation
    openbus: Cell<u8>,

    /// NMI request
    intreq_nmi: bool,

    /// Interrupt request
    intreq_int: bool,

    /// NMITIMEN register
    nmitimen: u8,

    apumock: RefCell<[u8; 4]>,

    /// Multiplication/division unit registers
    wrmpya: u8,
    wrdiv: u16,
    rddiv: u16,
    rdmpy: u16,

    /// H/V-interrupt registers
    last_scanline: usize,
    vtime: u16,
    htime: u16,
    timeup: Cell<bool>,
}

enum DMADirection {
    CPUToIO,
    IOToCPU,
}

enum DMAStep {
    Increment,
    Decrement,
    Fixed,
}

/// All parameters for a single DMA channel
#[derive(Copy, Clone, Debug)]
struct DMAChannel {
    /// Channel parameters
    dmap: u8,

    /// I/O-bus address
    bbad: u8,

    /// HDMA table start address / DMA current address
    a1t: u16,

    /// HDMA table start bank / DMA current bank
    a1b: u8,

    /// DMA byte-counter / Indirect HDMA address
    das: u16,

    /// DMA byte-counter / Indirect HDMA address (bank)
    dasb: u8,

    /// HDMA Table Current Address
    a2a: u16,

    /// HDMA Line-Counter (from current Table entry)
    ntrl: u8,

    /// Unused byte
    unused: u8,

    /// HDMA state: do transfer
    hdma_dotransfer: bool,

    /// HDMA state: repeat
    hdma_repeat: bool,

    /// HDMA state: line counter
    hdma_lines: u8,
}

impl DMAChannel {
    pub fn new() -> Self {
        Self {
            dmap: 0xFF,
            bbad: 0xFF,
            a1t: 0xFFFF,
            a1b: 0xFF,
            das: 0xFFFF,
            dasb: 0xFF,
            a2a: 0xFFFF,
            ntrl: 0xFF,
            unused: 0xFF,
            hdma_dotransfer: false,
            hdma_repeat: false,
            hdma_lines: 0,
        }
    }

    pub fn mode(&self) -> u8 {
        self.dmap & 0x07
    }

    pub fn direction(&self) -> DMADirection {
        if self.dmap & 1 << 7 == 0 {
            DMADirection::CPUToIO
        } else {
            DMADirection::IOToCPU
        }
    }

    pub fn step(&self) -> DMAStep {
        match (self.dmap >> 3) & 0x03 {
            0 => DMAStep::Increment,
            1 | 3 => DMAStep::Fixed,
            2 => DMAStep::Decrement,

            _ => unreachable!(),
        }
    }

    pub fn len(&self) -> Address {
        if self.das == 0 {
            0x10000
        } else {
            Address::from(self.das)
        }
    }

    pub fn b_addr(&self) -> Address {
        0x2100 | Address::from(self.bbad)
    }

    pub fn a_addr(&self) -> Address {
        Address::from(self.a1b) << 16 | Address::from(self.a1t)
    }

    pub fn hdma_current_a_addr(&self) -> Address {
        if self.hdma_is_indirect() {
            self.hdma_current_a_addr_indirect()
        } else {
            self.hdma_current_a_addr_direct()
        }
    }

    pub fn hdma_set_current_a_addr(&mut self, addr: Address) {
        if self.hdma_is_indirect() {
            self.dasb = (addr >> 16) as u8;
            self.das = addr as u16;
        } else {
            self.a1b = (addr >> 16) as u8;
            self.a1t = addr as u16;
        }
    }

    pub fn hdma_current_a_addr_direct(&self) -> Address {
        Address::from(self.a1b) << 16 | Address::from(self.a2a)
    }

    pub fn hdma_current_a_addr_indirect(&self) -> Address {
        Address::from(self.dasb) << 16 | Address::from(self.das)
    }

    pub fn hdma_is_indirect(&self) -> bool {
        self.dmap & (1 << 6) != 0
    }
}

impl<TRenderer> Mainbus<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn new(
        cartridge: &[u8],
        trace: BusTrace,
        renderer: TRenderer,
        joypads: [Joypad; JOYPAD_COUNT],
    ) -> Self {
        Self {
            cartridge: cartridge.to_owned(),
            wram: vec![0; WRAM_SIZE],
            trace,
            dma: [DMAChannel::new(); DMA_CHANNELS],
            hdmaen: 0,
            joypads,

            ppu: PPU::<TRenderer>::new(renderer),

            memsel: 0,
            wmadd: Cell::new(0),
            openbus: Cell::new(0),

            intreq_nmi: false,
            intreq_int: false,
            nmitimen: 0,

            apumock: RefCell::new([0xAA, 0, 0, 0]),

            wrmpya: 0,
            wrdiv: 0,
            rddiv: 0,
            rdmpy: 0,

            last_scanline: 0,
            htime: 0,
            vtime: 0,
            timeup: Cell::new(false),
        }
    }

    fn gdma_run(&mut self, chmask: u8) {
        for ch in 0..DMA_CHANNELS {
            if chmask & (1 << ch) == 0 {
                continue;
            }

            if self.hdmaen & (1 << ch) != 0 {
                println!("??? Doing GDMA on a channel ({}) enabled for HDMA", ch);
            }

            if self.trace == BusTrace::All {
                dbg_hex!(&self.dma[ch]);
            }

            for i in 0..self.dma[ch].len() {
                let b_addr = self.dma[ch].b_addr();
                let a_addr = match self.dma[ch].step() {
                    DMAStep::Increment => self.dma[ch].a_addr() + i,
                    DMAStep::Fixed => self.dma[ch].a_addr(),
                    _ => todo!(),
                };

                self.dma_transfer_step(ch, a_addr, b_addr, i);
            }
        }
    }

    fn dma_transfer_unit(&mut self, ch: usize, a_addr: Address, b_addr: Address) -> Address {
        let mut c = 0;
        while self.dma_transfer_step(ch, a_addr, b_addr, c) > 0 {
            c += 1;
        }
        c + 1
    }

    /// Transfers one BYTE of a DMA transfer unit
    fn dma_transfer_step(
        &mut self,
        ch: usize,
        a_addr: Address,
        b_addr: Address,
        unit_offset: Address,
    ) -> Address {
        match self.dma[ch].mode() {
            0 => {
                match self.dma[ch].direction() {
                    DMADirection::CPUToIO => {
                        let v = self.read(a_addr);
                        self.write(b_addr, v);
                    }
                    _ => todo!(),
                }
                0
            }
            1 => {
                let b_addr = b_addr + (unit_offset % 2);
                match self.dma[ch].direction() {
                    DMADirection::CPUToIO => {
                        let v = self.read(a_addr);
                        self.write(b_addr, v);
                    }
                    _ => todo!(),
                }
                1 - (unit_offset % 2)
            }
            2 => {
                match self.dma[ch].direction() {
                    DMADirection::CPUToIO => {
                        let v = self.read(a_addr);
                        self.write(b_addr, v);
                    }
                    _ => todo!(),
                }
                1 - (unit_offset % 2)
            }
            _ => panic!("Unimplemented DMA mode {}", self.dma[ch].mode()),
        }
    }

    fn hdma_reset(&mut self) {
        for ch in 0..DMA_CHANNELS {
            if self.hdmaen & (1 << ch) == 0 {
                continue;
            }

            // Reset current address to start address
            self.dma[ch].a2a = self.dma[ch].a1t;

            self.hdma_load_next_entry(ch);
        }
    }

    fn hdma_run(&mut self) {
        for ch in 0..DMA_CHANNELS {
            if self.hdmaen & (1 << ch) == 0 {
                continue;
            }

            // Current cycle ended?
            if !self.dma[ch].hdma_repeat && self.dma[ch].hdma_lines == 0 {
                continue;
            }

            if self.dma[ch].hdma_dotransfer {
                let a_addr = self.dma[ch].hdma_current_a_addr();
                let b_addr = self.dma[ch].b_addr();
                let len = self.dma_transfer_unit(ch, a_addr, b_addr);

                self.dma[ch].hdma_set_current_a_addr(a_addr.wrapping_add(len));
            }

            self.dma[ch].hdma_lines -= 1;
            self.dma[ch].hdma_dotransfer = self.dma[ch].hdma_repeat;

            if self.dma[ch].hdma_lines == 0 {
                // Next entry
                self.hdma_load_next_entry(ch);
            }
        }
    }

    fn hdma_load_next_entry(&mut self, ch: usize) {
        // Load flags (line count + repeat) from table
        let replc = self.read(self.dma[ch].hdma_current_a_addr());

        self.dma[ch].hdma_repeat = replc & (1 << 7) != 0;
        self.dma[ch].hdma_lines = replc & !(1 << 7);

        // Bump address to next table entry
        self.dma[ch].a2a = self.dma[ch].a2a.wrapping_add(1);

        if self.dma[ch].hdma_is_indirect() {
            let indirect_addr = self.read16(self.dma[ch].hdma_current_a_addr_direct());

            // Next entry
            self.dma[ch].a2a = self.dma[ch].a2a.wrapping_add(2);
            self.dma[ch].das = indirect_addr;
        }

        self.dma[ch].hdma_dotransfer = true;
    }
}

impl<TRenderer> Bus for Mainbus<TRenderer>
where
    TRenderer: Renderer,
{
    fn read(&self, fulladdr: Address) -> u8 {
        let (bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        let mapped_val = match bank {
            // System area
            0x00..=0x3F | 0x80..=0xBF => match addr {
                // WRAM mirror
                0x0000..=0x1FFF => Some(self.wram[addr]),
                // Picture Processing Unit
                0x2100..=0x213F => self.ppu.read(fulladdr),
                // APU comms
                0x2140..=0x217F => {
                    let ch = (addr - 0x2140) % 4;
                    let mut apumock = self.apumock.borrow_mut();
                    let value = apumock[ch];
                    apumock[ch] = match ch {
                        0 => 0xAA,
                        1 => 0xBB,
                        _ => 0,
                    };
                    Some(value)
                }
                // WMDATA - WRAM Data Read/Write (R/W)
                0x2180 => {
                    let addr = self.wmadd.get();
                    let val = self.wram[addr as usize];
                    self.wmadd.set(addr.wrapping_add(1) & WRAM_MASK);
                    Some(val)
                }
                // WMADDL/M/H - WRAM Address
                0x2181..=0x2183 => None,
                // JOYA - Joypad Input Register A (R)
                0x4016 => Some(self.joypads[0].read() | self.joypads[2].read() << 1),
                // JOYB - Joypad Input Register B (R)
                0x4017 => Some(self.joypads[1].read() | self.joypads[3].read() << 1 | 0x0C),
                // NMITIMEN - Interrupt Enable and Joypad Request (W)
                0x4200 => None,
                // WRMPYA - Set unsigned 8bit Multiplicand (W)
                0x4202 => None,
                // WRMPYB - Set unsigned 8bit Multiplier and Start Multiplication (W)
                0x4203 => None,
                // WRDIVL - Set unsigned 16bit Dividend (lower 8bit) (W)
                0x4204 => None,
                // WRDIVH - Set unsigned 16bit Dividend (upper 8bit) (W)
                0x4205 => None,
                // WRDIVB - Set unsigned 8bit Divisor and Start Division (W)
                0x4206 => None,
                // MDMAEN - Select General Purpose DMA Channel(s) and Start Transfer
                0x420B => Some(0xFF),
                // MEMSEL - Memory-2 Waitstate Control
                0x420D => Some(self.memsel),
                // RDNMI - V-Blank NMI Flag and CPU Version Number
                0x4210 => {
                    if self.ppu.in_vblank() {
                        Some(0x80 | 2 | (self.openbus.get() & 0x70))
                    } else {
                        Some(2 | (self.openbus.get() & 0x70))
                    }
                }
                // TIMEUP - H/V-Timer IRQ flag (R)
                0x4211 => {
                    let v = self.timeup.get();
                    self.timeup.set(false);
                    if v {
                        Some((self.openbus.get() & 0x7F) | 0x80)
                    } else {
                        Some(self.openbus.get() & 0x7F)
                    }
                }
                // HVBJOY - H/V-Blank flag and Joypad Busy flag (R)
                0x4212 => {
                    // TODO auto-joypad-read busy
                    let mut result = self.openbus.get() & 0x3E;
                    if self.ppu.in_vblank() {
                        result |= 1 << 7;
                    }
                    if self.ppu.in_hblank() {
                        result |= 1 << 6;
                    }

                    Some(result)
                }
                // RDDIVL - Unsigned Division Result (Quotient) (lower 8bit) (R)
                0x4214 => Some(self.rddiv as u8),
                // RDDIVH - Unsigned Division Result (Quotient) (upper 8bit) (R)
                0x4215 => Some((self.rddiv >> 8) as u8),
                // RDMPYL - Unsigned Division Remainder / Multiply Product (lo.8bit) (R)
                0x4216 => Some(self.rdmpy as u8),
                // RDMPYH - Unsigned Division Remainder / Multiply Product (up.8bit) (R)
                0x4217 => Some((self.rdmpy >> 8) as u8),
                // JOY1L/JOY1H - Joypad 1 (gameport 1, pin 4) (R)
                0x4218 => Some(self.joypads[0].read_auto_low()),
                0x4219 => Some(self.joypads[0].read_auto_high()),
                // JOY2L/JOY2H - Joypad 2 (gameport 2, pin 4) (R)
                0x421A => Some(self.joypads[1].read_auto_low()),
                0x421B => Some(self.joypads[1].read_auto_high()),
                // JOY3L/JOY3H - Joypad 3 (gameport 1, pin 5) (R)
                0x421C => Some(self.joypads[2].read_auto_low()),
                0x421D => Some(self.joypads[2].read_auto_high()),
                // JOY4L/JOY4H - Joypad 4 (gameport 2, pin 5) (R)
                0x421E => Some(self.joypads[3].read_auto_low()),
                0x421F => Some(self.joypads[3].read_auto_high()),
                // DMA parameter area
                0x4300..=0x43FF => {
                    let ch = (addr >> 4) & 0x07;
                    match addr & 0x0F {
                        // DMAPx - DMA/HDMA parameters
                        0x00 => Some(self.dma[ch].dmap),
                        // BBADx - DMA/HDMA I/O-Bus Address
                        0x01 => Some(self.dma[ch].bbad),
                        // A1TxL - HDMA Table Start Address (low)  / DMA Curr Addr (low)
                        0x02 => Some(self.dma[ch].a1t as u8),
                        // A1TxH - HDMA Table Start Address (high)  / DMA Curr Addr (high)
                        0x03 => Some((self.dma[ch].a1t >> 8) as u8),
                        // A1TxB - HDMA Table Start Address (bank) / DMA Curr Addr (bank)
                        0x04 => Some(self.dma[ch].a1b),
                        // DASxL - Indirect HDMA Address (low)  / DMA Byte-Counter (low)
                        0x05 => Some(self.dma[ch].das as u8),
                        // DASxH - Indirect HDMA Address (high)  / DMA Byte-Counter (high)
                        0x06 => Some((self.dma[ch].das >> 8) as u8),
                        // DASxB - Indirect HDMA Address (bank)
                        0x07 => Some(self.dma[ch].dasb),
                        // A2AxL - HDMA Table Current Address (low) (R/W)
                        0x08 => Some(self.dma[ch].a2a as u8),
                        // A2AxH - HDMA Table Current Address (high) (R/W)
                        0x09 => Some((self.dma[ch].a2a >> 8) as u8),
                        // NTRLx - HDMA Line-Counter (from current Table entry) (R/W)
                        0x0A => Some(self.dma[ch].ntrl),
                        // UNUSEDx - Unused Byte (R/W)
                        0x0B | 0x0F => Some(self.dma[ch].unused),

                        _ => None,
                    }
                }
                // WS1/2 LoROM
                0x8000..=0xFFFF => Some(self.cartridge[addr - 0x8000 + (bank & !0x80) * 0x8000]),

                _ => None,
            },
            // WS1 HiROM
            0x40..=0x7D => {
                Some(self.cartridge[(addr + ((bank - 0x40) * 0x10000)) % self.cartridge.len()])
            }
            // Full WRAM area
            0x7E..=0x7F => Some(self.wram[((bank - 0x7E) * WRAM_BANK_SIZE) + addr]),
            // WS2 HiROM
            0xC0..=0xFF => Some(self.cartridge[addr + ((bank - 0xC0) * 0x10000)]),
            _ => None,
        };

        if let Some(v) = mapped_val {
            if self.trace == BusTrace::All {
                println!("Bus read: {:06X} = {:02X}", fulladdr, v);
            }
            self.openbus.set(v);
            v
        } else {
            // Open bus
            if self.trace != BusTrace::None {
                println!("Open/unimplemented bus read: {:06X}", fulladdr);
            }

            self.openbus.get()
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
                // APU comms
                0x2140..=0x217F => {
                    let ch = (addr - 0x2140) % 4;
                    let mut apumock = self.apumock.borrow_mut();
                    apumock[ch] = val;
                    Some(())
                }
                // WMDATA - WRAM Data Read/Write
                0x2180 => {
                    let addr = self.wmadd.get();
                    self.wram[addr as usize] = val;
                    self.wmadd.set(addr.wrapping_add(1) & WRAM_MASK);
                    Some(())
                }
                // WMADDL - WRAM Address
                0x2181 => {
                    let addr = (self.wmadd.get() & !0xFF) | Address::from(val);
                    self.wmadd.set(addr & WRAM_MASK);
                    Some(())
                }
                // WMADDM - WRAM Address
                0x2182 => {
                    let addr = (self.wmadd.get() & !0xFF00) | (Address::from(val) << 8);
                    self.wmadd.set(addr & WRAM_MASK);
                    Some(())
                }
                // WMADDH - WRAM Address
                0x2183 => {
                    let addr = (self.wmadd.get() & !0xFF0000) | (Address::from(val) << 16);
                    self.wmadd.set(addr & WRAM_MASK);
                    Some(())
                }
                // HTIMEL/HTIMEH - H-Count timer setting (W)
                0x4207 => Some(self.htime = (self.htime & 0xFF00) | val as u16),
                0x4208 => Some(self.htime = ((self.htime & 0x00FF) | (val as u16) << 8) & 0x01),
                // VTIMEL/VTIMEH - V-Count timer setting (W)
                0x4209 => Some(self.vtime = (self.vtime & 0xFF00) | val as u16),
                0x420A => Some(self.vtime = ((self.vtime & 0x00FF) | (val as u16) << 8) & 0x01),
                // JOYWR - Joypad Output (W)
                0x4016 => Some(self.joypads.iter_mut().for_each(|j| j.strobe())),
                // NMITIMEN - Interrupt Enable and Joypad Request (W)
                0x4200 => {
                    // TODO joypad
                    if val & 0x30 != self.nmitimen & 0x30 {
                        println!(
                            "H/V interrupts: {:02X} - VTIME {}, HTIME {}",
                            (val >> 4) & 0x03,
                            self.vtime,
                            self.htime
                        );
                        if (val >> 4) & 0x03 == 0 {
                            // TIMEUP gets cleared when disabling H/V interrupts
                            self.timeup.set(false);
                        }
                    }
                    if val & 0x80 != self.nmitimen & 0x80 {
                        if val & 0x80 != 0 {
                            println!("VBlank NMIs enabled");
                        } else {
                            println!("VBlank NMIs disabled");
                        }
                    }
                    Some(self.nmitimen = val)
                }
                // WRMPYA - Set unsigned 8bit Multiplicand (W)
                0x4202 => Some(self.wrmpya = val),
                // WRMPYB - Set unsigned 8bit Multiplier and Start Multiplication (W)
                0x4203 => {
                    let result = u16::from(self.wrmpya).wrapping_mul(val as u16);

                    // This is a hardware quirk, multiplication destroys
                    // the division result.
                    self.rddiv = val as u16;

                    Some(self.rdmpy = result)
                }
                // WRDIVL - Set unsigned 16bit Dividend (lower 8bit) (W)
                0x4204 => Some(self.wrdiv = (self.wrdiv & 0xFF00) | (val as u16)),
                // WRDIVH - Set unsigned 16bit Dividend (upper 8bit) (W)
                0x4205 => Some(self.wrdiv = (self.wrdiv & 0x00FF) | ((val as u16) << 8)),
                // WRDIVB - Set unsigned 8bit Divisor and Start Division (W)
                0x4206 => {
                    if val == 0 {
                        println!("Division by zero: {} / {}", self.wrdiv, val);
                        self.rddiv = 0xFFFF;
                        self.rdmpy = self.wrdiv;
                    } else {
                        self.rddiv = self.wrdiv / val as u16;
                        self.rdmpy = self.wrdiv.rem_euclid(val as u16);
                    }
                    Some(())
                }
                // MDMAEN - Select General Purpose DMA Channel(s) and Start Transfer
                0x420B => Some(self.gdma_run(val)),
                // HDMAEN - Select H-Blank DMA (H-DMA) Channel(s) (W)
                0x420C => Some(self.hdmaen = val),
                // MEMSEL - Memory-2 Waitstate Control
                0x420D => Some(self.memsel = val),
                // RDDIVL - Unsigned Division Result (Quotient) (lower 8bit) (R)
                0x4214 => None,
                // RDDIVH - Unsigned Division Result (Quotient) (upper 8bit) (R)
                0x4215 => None,
                // RDMPYL - Unsigned Division Remainder / Multiply Product (lo.8bit) (R)
                0x4216 => None,
                // RDMPYH - Unsigned Division Remainder / Multiply Product (up.8bit) (R)
                0x4217 => None,
                // DMA parameter area
                0x4300..=0x43FF => {
                    let ch = (addr >> 4) & 0x07;
                    match addr & 0x0F {
                        // DMAPx - DMA/HDMA parameters
                        0x00 => Some(self.dma[ch].dmap = val),
                        // BBADx - DMA/HDMA I/O-Bus Address
                        0x01 => Some(self.dma[ch].bbad = val),
                        // A1TxL - HDMA Table Start Address (low)
                        // DMA Curr Addr (low)
                        0x02 => Some(self.dma[ch].a1t = (self.dma[ch].a1t & 0xFF00) | val as u16),
                        // A1TxH - HDMA Table Start Address (high)
                        // DMA Curr Addr (high)
                        0x03 => Some(
                            self.dma[ch].a1t = (self.dma[ch].a1t & 0x00FF) | ((val as u16) << 8),
                        ),
                        // A1TxB - HDMA Table Start Address (bank)
                        // DMA Curr Addr (bank)
                        0x04 => Some(self.dma[ch].a1b = val),
                        // DASxL - Indirect HDMA Address (low)
                        // DMA Byte-Counter (low)
                        0x05 => Some(self.dma[ch].das = (self.dma[ch].das & 0xFF00) | val as u16),
                        // DASxH - Indirect HDMA Address (high)
                        // DMA Byte-Counter (high)
                        0x06 => Some(
                            self.dma[ch].das = (self.dma[ch].das & 0x00FF) | ((val as u16) << 8),
                        ),
                        // DASxB - Indirect HDMA Address (bank)
                        0x07 => Some(self.dma[ch].dasb = val),
                        // A2AxL - HDMA Table Current Address (low) (R/W)
                        0x08 => Some(self.dma[ch].a2a = (self.dma[ch].a2a & 0xFF00) | val as u16),
                        // A2AxH - HDMA Table Current Address (high) (R/W)
                        0x09 => {
                            Some(self.dma[ch].a2a = (self.dma[ch].a2a & 0x00FF) | (val as u16) << 8)
                        }
                        // NTRLx - HDMA Line-Counter (from current Table entry) (R/W)
                        0x0A => Some(self.dma[ch].ntrl = val),
                        // UNUSEDx - Unused Byte (R/W)
                        0x0B | 0x0F => Some(self.dma[ch].unused = val),

                        _ => None,
                    }
                }

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

    fn get_clr_nmi(&mut self) -> bool {
        let v = self.intreq_nmi;
        self.intreq_nmi = false;
        v
    }

    fn get_clr_int(&mut self) -> bool {
        let v = self.intreq_int;
        self.intreq_int = false;
        v
    }
}

impl<TRenderer> Tickable for Mainbus<TRenderer>
where
    TRenderer: Renderer,
{
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
        // This ratio is not based on anything that makes sense yet
        self.ppu.tick(ticks * 8)?;

        let entered_vblank = self.ppu.get_clr_intreq_vblank();
        let entered_hblank = self.ppu.get_clr_intreq_hblank();

        if entered_hblank && !self.ppu.in_vblank() {
            if self.ppu.get_current_scanline() == 0 {
                self.hdma_reset();
            }

            self.hdma_run();
        }

        if entered_vblank && self.nmitimen & (1 << 7) != 0 {
            self.intreq_nmi = true;
        }

        // H/V interrupt
        // TODO stop at EXACT H rather than at 0
        let hvint = match (self.nmitimen >> 4) & 0x03 {
            // Disabled
            0 => false,
            // H=H + V=*
            1 => self.ppu.get_current_scanline() != self.last_scanline,
            // H=0, V=V (2) + H=H, V=V (3)
            2 | 3 => {
                self.ppu.get_current_scanline() != self.last_scanline
                    && self.ppu.get_current_scanline() == usize::from(self.vtime)
            }
            _ => unreachable!(),
        };
        self.last_scanline = self.ppu.get_current_scanline();
        if hvint {
            self.intreq_int = true;
            self.timeup.set(true);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::NullRenderer;

    fn mainbus() -> Mainbus<NullRenderer> {
        let (joypads, _) = Joypad::new_channel_all();
        Mainbus::<NullRenderer>::new(
            &[],
            BusTrace::All,
            NullRenderer::new(0, 0).unwrap(),
            joypads,
        )
    }

    #[test]
    fn mainbus_wramport_inc() {
        let mut bus = mainbus();
        bus.write(0x2180, 0xAA);
        assert_eq!(bus.wram[0], 0xAA);
        assert_eq!(bus.wmadd.get(), 0x01);

        // Test wrap-around
        bus.write(0x2181, 0xFF);
        bus.write(0x2182, 0xFF);
        bus.write(0x2183, 0x01);
        assert_eq!(bus.wmadd.get(), 0x1FFFF);
        bus.write(0x2180, 0xBB);
        assert_eq!(bus.wram[0x1FFFF], 0xBB);
        assert_eq!(bus.wmadd.get(), 0x00);
    }

    #[test]
    fn mainbus_wramport_addr() {
        let mut bus = mainbus();
        bus.write(0x2181, 0x33);
        assert_eq!(bus.wmadd.get(), 0x33);
        bus.write(0x2182, 0x22);
        assert_eq!(bus.wmadd.get(), 0x2233);
        bus.write(0x2183, 0x11); // masked
        assert_eq!(bus.wmadd.get(), 0x12233);
        bus.write(0x2181, 0xBB);
        assert_eq!(bus.wmadd.get(), 0x122BB);
        bus.write(0x2182, 0xAA);
        assert_eq!(bus.wmadd.get(), 0x1AABB);
        bus.write(0x2183, 0x00); // masked
        assert_eq!(bus.wmadd.get(), 0xAABB);
    }

    #[test]
    fn mainbus_openbus() {
        let mut bus = mainbus();
        assert_eq!(bus.read(0x002000), 0x00);
        bus.write(0x000000, 0xFF);
        bus.read(0x000000);
        assert_eq!(bus.read(0x002000), 0xFF);
    }

    #[test]
    fn mainbus_openbus_rdnmi() {
        let mut bus = mainbus();
        assert_eq!(bus.read(0x004210), 0x02);
        bus.write(0x000000, 0xFF);
        bus.read(0x000000);
        assert_eq!(bus.read(0x004210), 0x72);
    }

    #[test]
    fn multiply() {
        let mut bus = mainbus();

        let mut mul = |a, b| {
            bus.write(0x4202, a);
            bus.write(0x4203, b);
            let l = bus.read(0x4216) as u16;
            let h = bus.read(0x4217) as u16;

            // Div results hardware quirk
            assert_eq!(bus.read(0x4214), b);
            assert_eq!(bus.read(0x4215), 0);
            h << 8 | l
        };

        assert_eq!(mul(10, 12), 120);
        assert_eq!(mul(0, 12), 0);
        assert_eq!(mul(1, 12), 12);
        assert_eq!(mul(255, 255), 65025);
    }

    #[test]
    fn divide() {
        let mut bus = mainbus();

        let mut div = |a, b| {
            bus.write16(0x4204, a);
            bus.write(0x4206, b);
            let l = bus.read(0x4214) as u16;
            let h = bus.read(0x4215) as u16;
            let rl = bus.read(0x4216) as u16;
            let rh = bus.read(0x4217) as u16;

            (h << 8 | l, rh << 8 | rl)
        };

        assert_eq!(div(12, 10), (1, 2));
        assert_eq!(div(10, 10), (1, 0));
        assert_eq!(div(65535, 255), (257, 0));
        assert_eq!(div(65500, 255), (256, 220));
    }

    #[test]
    fn divide_by_zero() {
        let mut bus = mainbus();

        let mut div = |a, b| {
            bus.write16(0x4204, a);
            bus.write(0x4206, b);
            let l = bus.read(0x4214) as u16;
            let h = bus.read(0x4215) as u16;
            let rl = bus.read(0x4216) as u16;
            let rh = bus.read(0x4217) as u16;

            (h << 8 | l, rh << 8 | rl)
        };

        assert_eq!(div(123, 0), (0xFFFF, 123));
        assert_eq!(div(0x1234, 0), (0xFFFF, 0x1234));
    }
}
