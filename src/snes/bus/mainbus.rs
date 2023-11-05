use std::cell::{Cell, RefCell};

use anyhow::Result;
use dbg_hex::dbg_hex;

use crate::frontend::Renderer;
use crate::snes::bus::{Address, Bus, BusMember};
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
    trace: BusTrace,

    /// Picture Processing Unit
    ppu: PPU<TRenderer>,

    /// MEMSEL - Memory-2 Waitstate Control
    memsel: u8,

    /// DMA channels
    dma: [DMAChannel; DMA_CHANNELS],

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
            Address::from(self.das + (self.das % 4))
        }
    }

    pub fn b_addr(&self) -> Address {
        0x2100 | Address::from(self.bbad)
    }

    pub fn a_addr(&self) -> Address {
        Address::from(self.a1b) << 16 | Address::from(self.a1t)
    }
}

impl<TRenderer> Mainbus<TRenderer>
where
    TRenderer: Renderer,
{
    pub fn new(cartridge: &[u8], trace: BusTrace, renderer: TRenderer) -> Self {
        Self {
            cartridge: cartridge.to_owned(),
            wram: vec![0; WRAM_SIZE],
            trace,
            dma: [DMAChannel::new(); DMA_CHANNELS],

            ppu: PPU::<TRenderer>::new(renderer),

            memsel: 0,
            wmadd: Cell::new(0),
            openbus: Cell::new(0),

            intreq_nmi: false,
            intreq_int: false,
            nmitimen: 0,

            apumock: RefCell::new([0xAA, 0, 0, 0]),
        }
    }

    fn gdma_run(&mut self, chmask: u8) {
        for ch in 0..DMA_CHANNELS {
            if chmask & (1 << ch) == 0 {
                continue;
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
                match self.dma[ch].mode() {
                    0 => match self.dma[ch].direction() {
                        DMADirection::CPUToIO => {
                            let v = self.read(a_addr);
                            self.write(b_addr, v);
                        }
                        _ => todo!(),
                    },
                    1 => {
                        let b_addr = b_addr + (i & 1);
                        match self.dma[ch].direction() {
                            DMADirection::CPUToIO => {
                                let v = self.read(a_addr);
                                self.write(b_addr, v);
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
        }
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
                // NMITIMEN - Interrupt Enable and Joypad Request (W)
                0x4200 => None,
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
                // JOYxx - Joypads
                0x4218..=0x421F => Some(0),
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

                        _ => None,
                    }
                }
                // WS1/2 LoROM
                0x8000..=0xFFFF => Some(self.cartridge[addr - 0x8000 + (bank & !0x80) * 0x8000]),

                _ => None,
            },
            // WS1 HiROM
            0x40..=0x7D => Some(self.cartridge[addr + ((bank - 0x40) * 0x10000)]),
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
                // NMITIMEN - Interrupt Enable and Joypad Request (W)
                0x4200 => {
                    // TODO joypad
                    if val & 0x30 != self.nmitimen & 0x30 {
                        // TODO H/V interrupts
                        println!("H/V interrupts: {:02X}", val & 0x30);
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
                // MDMAEN - Select General Purpose DMA Channel(s) and Start Transfer
                0x420B => Some(self.gdma_run(val)),
                // MEMSEL - Memory-2 Waitstate Control
                0x420D => Some(self.memsel = val),
                // DMA parameter area
                0x4300..=0x43FF => {
                    let ch = (addr >> 4) & 0x07;
                    match addr & 0x0F {
                        // DMAPx - DMA/HDMA parameters
                        0x00 => Some(self.dma[ch].dmap = val),
                        // BBADx - DMA/HDMA I/O-Bus Address
                        0x01 => Some(self.dma[ch].bbad = val),
                        // A1TxL - HDMA Table Start Address (low)  / DMA Curr Addr (low)
                        0x02 => Some(self.dma[ch].a1t = (self.dma[ch].a1t & 0xFF00) | val as u16),
                        // A1TxH - HDMA Table Start Address (high)  / DMA Curr Addr (high)
                        0x03 => Some(
                            self.dma[ch].a1t = (self.dma[ch].a1t & 0x00FF) | ((val as u16) << 8),
                        ),
                        // A1TxB - HDMA Table Start Address (bank) / DMA Curr Addr (bank)
                        0x04 => Some(self.dma[ch].a1b = val),
                        // DASxL - Indirect HDMA Address (low)  / DMA Byte-Counter (low)
                        0x05 => Some(self.dma[ch].das = (self.dma[ch].das & 0xFF00) | val as u16),
                        // DASxH - Indirect HDMA Address (high)  / DMA Byte-Counter (high)
                        0x06 => Some(
                            self.dma[ch].das = (self.dma[ch].das & 0x00FF) | ((val as u16) << 8),
                        ),
                        // DASxB - Indirect HDMA Address (bank)
                        0x07 => Some(self.dma[ch].dasb = val),

                        _ => todo!(),
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
        self.ppu.tick(ticks)?;

        if self.ppu.get_clr_intreq_vblank() && self.nmitimen & (1 << 7) != 0 {
            self.intreq_nmi = true;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::NullRenderer;

    fn mainbus() -> Mainbus<NullRenderer> {
        Mainbus::<NullRenderer>::new(&[], BusTrace::All, NullRenderer::new(0, 0).unwrap())
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
}
