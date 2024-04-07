use super::state::PPUState;

use anyhow::Result;
use rusty_pool::ThreadPool;
use serde::{Deserialize, Serialize};

use crate::bus::{Address, BusMember};
use crate::frontend::Renderer;
use crate::snes::cartridge::VideoFormat;
use crate::tickable::{Tickable, Ticks};

use std::cell::Cell;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::thread::sleep;
use std::time::{Duration, Instant};

pub const SCREEN_WIDTH: usize = 8 * 32;
pub const SCREEN_HEIGHT: usize = 8 * 28;

// The entire screen should be shifted up by 1 scanline
const SCANLINE_OUTPUT_OFFSET: isize = -1;

/// Type used for VRAM storage, thread safe
pub type Vram = Arc<InnerVram>;
pub type InnerVram = Vec<VramWord>;

pub type VramWord = u16;
pub const VRAM_WORDS: usize = 32 * 1024;
pub const VRAM_WORDSIZE: usize = 2;
// 32K-words addressable (64KB)
pub const VRAM_ADDRMASK: usize = VRAM_WORDS - 1;

// VMAIN bits
const VMAIN_HIGH: u8 = 1 << 7;
const VMAIN_INC_MASK: u8 = 0x03;
const VMAIN_TRANSLATE_MASK: u8 = 0x03;
const VMAIN_TRANSLATE_SHIFT: u8 = 2;

fn _default_none<T>() -> Option<T> {
    None
}

fn _default_now() -> Instant {
    Instant::now()
}

#[derive(Serialize, Deserialize)]
pub struct PPU<TRenderer: Renderer> {
    pub(super) vram: InnerVram,

    #[serde(skip, default = "_default_none")]
    pub renderer: Option<TRenderer>,

    videoformat: VideoFormat,

    /// Timestamp when the last frame was completed
    #[serde(skip, default = "_default_now")]
    last_frame: Instant,

    /// The desired speed at which to run the PPU (and with that the emulator)
    /// in microseconds per frame.
    desired_frametime: u64,

    pub(super) cycles: usize,
    pub(super) last_scanline: usize,
    pub(super) intreq_vblank: bool,
    pub(super) intreq_hblank: bool,
    pub(super) vblank: bool,
    pub(super) hblank: bool,

    state: PPUState,

    #[serde(skip)]
    pool: ThreadPool,

    // H/V latches
    /// Current value latched in OPHCT
    pub(super) hlatch: Cell<u16>,
    /// Current value latched in OPVCT
    pub(super) vlatch: Cell<u16>,
    /// OPHCT MSB read flip-flop
    pub(super) hlatch_msb: Cell<bool>,
    /// OPVCT MSB read flip-flop
    pub(super) vlatch_msb: Cell<bool>,
    /// H/V latched indicator, for STAT78.6
    pub(super) hvlatched: Cell<bool>,

    pub(super) vmadd: Cell<u16>,
    pub(super) vmain: u8,
    pub(super) vram_prefetch: Cell<u16>,

    pub(super) interlace_frame: bool,
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    const CYCLES_PER_SCANLINE: usize = 341; // including H-blank
    const SCANLINES_PER_FRAME: usize = 262; // including V-blank
    const VBLANK_START: usize = 225;
    const LINE_HBLANK_START: usize = 274;

    pub fn new(renderer: TRenderer, fps: u64, videoformat: VideoFormat) -> Self {
        let desired_frametime = if fps == 0 { 0 } else { 1_000_000 / fps };

        Self {
            vram: vec![0; VRAM_WORDS],

            renderer: Some(renderer),
            videoformat,
            cycles: 0,
            last_scanline: 0,
            intreq_vblank: false,
            intreq_hblank: false,
            vblank: false,
            hblank: false,

            state: PPUState::new(),
            pool: ThreadPool::default(),

            hlatch: Cell::new(0),
            vlatch: Cell::new(0),
            hlatch_msb: Cell::new(false),
            vlatch_msb: Cell::new(false),
            hvlatched: Cell::new(false),

            vmadd: Cell::new(0),
            vmain: 0,
            vram_prefetch: Cell::new(0),
            interlace_frame: false,

            last_frame: Instant::now(),
            desired_frametime,
        }
    }

    pub fn single_threaded(&mut self) {
        self.pool = ThreadPool::new(1, 1, Duration::from_secs(60));
    }

    pub fn set_fps_limit(&mut self, fps: u64) {
        self.desired_frametime = if fps == 0 { 0 } else { 1_000_000 / fps };
    }

    pub fn get_current_scanline(&self) -> usize {
        self.cycles / Self::CYCLES_PER_SCANLINE
    }

    pub fn get_current_h(&self) -> usize {
        self.cycles % Self::CYCLES_PER_SCANLINE
    }

    pub fn in_vblank(&self) -> bool {
        self.last_scanline >= Self::VBLANK_START
    }

    pub fn in_hblank(&self) -> bool {
        self.cycles % Self::CYCLES_PER_SCANLINE >= Self::LINE_HBLANK_START
    }

    pub fn get_clr_intreq_vblank(&mut self) -> bool {
        let v = self.intreq_vblank;
        self.intreq_vblank = false;
        v
    }

    pub fn get_clr_intreq_hblank(&mut self) -> bool {
        let v = self.intreq_hblank;
        self.intreq_hblank = false;
        v
    }

    pub fn render_scanline(&mut self, scanline: usize, output_offset: isize) {
        let output_line = usize::try_from((scanline as isize) + output_offset).unwrap();
        if output_line >= SCREEN_HEIGHT {
            return;
        }
        let mut t_state = self.state.clone();
        let t_buffer = self.renderer.as_mut().unwrap().get_buffer();

        self.pool.execute(move || {
            let line = t_state.render_scanline(scanline);
            for (x, color) in line.into_iter().enumerate() {
                let idx = ((output_line * SCREEN_WIDTH) + x) * 4;
                t_buffer[idx].store(color.2.into(), Ordering::Release);
                t_buffer[idx + 1].store(color.1.into(), Ordering::Release);
                t_buffer[idx + 2].store(color.0.into(), Ordering::Release);
            }
        });
    }

    pub(super) fn vram_autoinc(&self, upper: bool) {
        let inc_on_upper = self.vmain & VMAIN_HIGH != 0;
        if upper != inc_on_upper {
            return;
        }

        // Prefetch glitch: prefetch is updated BEFORE the address
        self.vram_update_prefetch();

        let inc = match self.vmain & VMAIN_INC_MASK {
            0 => 1,
            1 => 32,
            2..=3 => 128,
            _ => unreachable!(),
        };
        self.vmadd.set(self.vmadd.get().wrapping_add(inc));
    }

    pub(super) fn vram_update_prefetch(&self) {
        let addr = self.vram_addr_translate(self.vmadd.get());

        self.vram_prefetch
            .set(self.vram[addr as usize & VRAM_ADDRMASK]);
    }

    pub(super) fn vram_addr_translate(&self, addr: u16) -> u16 {
        // Translation  Bitmap Type              Port [2116h/17h]     VRAM Word-Address
        //  8bit rotate  4-color; 1 word/plane   aaaaaaaaYYYxxxxx --> aaaaaaaaxxxxxYYY
        //  9bit rotate  16-color; 2 words/plane aaaaaaaYYYxxxxxP --> aaaaaaaxxxxxPYYY
        // 10bit rotate 256-color; 4 words/plane aaaaaaYYYxxxxxPP --> aaaaaaxxxxxPPYYY
        match (self.vmain >> VMAIN_TRANSLATE_SHIFT) & VMAIN_TRANSLATE_MASK {
            0 => addr,
            1 => (addr & 0xFF00) | ((addr << 3) & 0x00F8) | ((addr >> 5) & 0x07),
            2 => (addr & 0xFE00) | ((addr << 3) & 0x01F8) | ((addr >> 6) & 0x07),
            3 => (addr & 0xFC00) | ((addr << 3) & 0x03F8) | ((addr >> 7) & 0x07),
            _ => unreachable!(),
        }
    }
}

impl<TRenderer> Tickable for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn tick(&mut self, ticks: Ticks) -> Result<Ticks> {
        self.cycles =
            (self.cycles + ticks) % (Self::CYCLES_PER_SCANLINE * Self::SCANLINES_PER_FRAME);

        if self.in_hblank() {
            if !self.hblank {
                // Entered HBlank
                self.hblank = true;
                self.intreq_hblank = true;
            }
        } else {
            self.hblank = false;
        }

        if self.get_current_scanline() != self.last_scanline {
            // Vertically progressed into a new scanline
            self.last_scanline = self.get_current_scanline();

            if self.in_vblank() {
                if !self.vblank {
                    // Entered VBlank
                    self.vblank = true;
                    self.intreq_vblank = true;

                    // Reload OAMADD
                    self.state.oamadd_addr.set(self.state.oamadd_reload.get());
                }
            } else {
                if self.vblank {
                    // VBlank period has ended
                    self.vblank = false;

                    // Toggle interlace frame bit in STAT78
                    self.interlace_frame = !self.interlace_frame;

                    // Roll over the VRAM buffer so any changes during the last frame
                    // reflect in the next frame.
                    self.state.vram = Arc::new(self.vram.clone());

                    // Send frame to the screen
                    // Wait for threadpool workers to finish all scanlines
                    self.pool.join();

                    // Present frame to the screen
                    let renderer = self.renderer.as_mut().unwrap();
                    renderer.update()?;

                    // Sync to desired framerate
                    let frametime = self.last_frame.elapsed().as_micros() as u64;
                    if frametime < self.desired_frametime {
                        sleep(Duration::from_micros(self.desired_frametime - frametime));
                    }
                    self.last_frame = Instant::now();
                }

                // Scanline 0 is discarded by the original hardware, so
                // scanline 1 becomes the top of the frame. However, H/V-interrupts,
                // HDMA, etc are still executed for scanline 0, so we only discard
                // it here and shift the whole frame up by 1.
                if (self.last_scanline as isize)
                    < ((SCREEN_HEIGHT as isize) - SCANLINE_OUTPUT_OFFSET)
                    && (self.last_scanline as isize) + SCANLINE_OUTPUT_OFFSET >= 0
                {
                    self.render_scanline(self.last_scanline, SCANLINE_OUTPUT_OFFSET);
                }
            }
        }

        Ok(ticks)
    }
}

impl<TRenderer> BusMember<Address> for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            // VMAIN - VRAM Address Increment Mode
            0x2115 => Some(self.vmain),
            // VMADDL - VRAM Address (lower 8bit)
            0x2116 => Some(self.vmadd.get() as u8),
            // VMADDH - VRAM Address (upper 8bit)
            0x2117 => Some((self.vmadd.get() >> 8) as u8),
            // RDVRAML - VRAM Data Read (lower 8bit)
            0x2139 => {
                let v = self.vram_prefetch.get() as u8;
                self.vram_autoinc(false);
                Some(v)
            }
            // RDVRAMH - VRAM Data Read (upper 8bit)
            0x213A => {
                let v = (self.vram_prefetch.get() >> 8) as u8;
                self.vram_autoinc(true);
                Some(v)
            }
            // SLHV - Latch H/V-Counter by Software (R)
            0x2137 => {
                // Note: flip-flops are not reset when latching occurs
                self.hlatch.set(self.get_current_h() as u16);
                self.vlatch.set(self.get_current_scanline() as u16);
                self.hvlatched.set(true);

                // Read openbus
                None
            }
            // OPHCT - Horizontal Counter Latch (R)
            0x213C => {
                let v = if !self.hlatch_msb.get() {
                    self.hlatch.get() as u8
                } else {
                    (self.hlatch.get() >> 8) as u8
                };
                self.hlatch_msb.set(!self.hlatch_msb.get());

                Some(v)
            }
            // OPVCT - Vertical Counter Latch (R)
            0x213D => {
                let v = if !self.vlatch_msb.get() {
                    self.vlatch.get() as u8
                } else {
                    (self.vlatch.get() >> 8) as u8
                };
                self.vlatch_msb.set(!self.vlatch_msb.get());

                Some(v)
            }
            // STAT78 - PPU2 Status and Version Number (R)
            0x213F => {
                // PPU2 version
                let mut val = 1;

                if self.interlace_frame {
                    val |= 1 << 7;
                }
                if self.hvlatched.get() {
                    val |= 1 << 6;
                }
                // Bit 5 is open bus
                val |= match self.videoformat {
                    VideoFormat::NTSC => 3,
                    VideoFormat::PAL => (1 << 4) | 3,
                };

                // Read clears all latches
                self.hlatch_msb.set(false);
                self.vlatch_msb.set(false);
                self.hvlatched.set(false);

                Some(val)
            }
            _ => self.state.read(fulladdr),
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            // VMAIN - VRAM Address Increment Mode
            0x2115 => Some(self.vmain = val),
            // VMADDL - VRAM Address (lower 8bit)
            0x2116 => {
                let v = self.vmadd.get() & 0xFF00;
                self.vmadd.set(v | val as u16);
                self.vram_update_prefetch();
                Some(())
            }
            // VMADDH - VRAM Address (upper 8bit)
            0x2117 => {
                let v = self.vmadd.get() & 0x00FF;
                self.vmadd.set(v | (val as u16) << 8);
                self.vram_update_prefetch();
                Some(())
            }
            // VMDATAL - VRAM Data write (lower 8bit)
            0x2118 => {
                let addr = usize::from(self.vram_addr_translate(self.vmadd.get())) & VRAM_ADDRMASK;
                self.vram_autoinc(false);

                let cur = self.vram[addr];
                Some(self.vram[addr] = (cur & 0xFF00) | val as u16)
            }
            // VMDATAH - VRAM Data write (upper 8bit)
            0x2119 => {
                let addr = usize::from(self.vram_addr_translate(self.vmadd.get())) & VRAM_ADDRMASK;
                self.vram_autoinc(true);

                let cur = self.vram[addr];
                Some(self.vram[addr] = (cur & 0xFF) | (val as u16) << 8)
            }
            _ => self.state.write(fulladdr, val),
        }
    }
}
