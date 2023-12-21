use super::state::PPUState;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::frontend::Renderer;
use crate::snes::bus::{Address, BusMember};
use crate::tickable::{Tickable, Ticks};

use std::cell::Cell;

pub const SCREEN_WIDTH: usize = 8 * 32;
pub const SCREEN_HEIGHT: usize = 8 * 28;

// The entire screen should be shifted up by 1 scanline
const SCANLINE_OUTPUT_OFFSET: isize = -1;

fn _default_none<T>() -> Option<T> {
    None
}

#[derive(Serialize, Deserialize)]
pub struct PPU<TRenderer: Renderer> {
    #[serde(skip, default = "_default_none")]
    pub renderer: Option<TRenderer>,

    pub(super) cycles: usize,
    pub(super) last_scanline: usize,
    pub(super) intreq_vblank: bool,
    pub(super) intreq_hblank: bool,
    pub(super) vblank: bool,
    pub(super) hblank: bool,

    state: PPUState,

    // H/V latches
    pub(super) hlatch: Cell<u8>,
    pub(super) vlatch: Cell<u8>,
}

impl<TRenderer> PPU<TRenderer>
where
    TRenderer: Renderer,
{
    const CYCLES_PER_SCANLINE: usize = 340 * 4;
    const SCANLINES_PER_FRAME: usize = 262; // including V-blank
    const VBLANK_START: usize = 0xE1;
    const LINE_HBLANK_START: usize = 274 * 4;

    pub fn new(renderer: TRenderer) -> Self {
        Self {
            renderer: Some(renderer),
            cycles: 0,
            last_scanline: 0,
            intreq_vblank: false,
            intreq_hblank: false,
            vblank: false,
            hblank: false,

            state: PPUState::new(),

            hlatch: Cell::new(0),
            vlatch: Cell::new(0),
        }
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
        let line = self.state.render_scanline(scanline);

        let output_line = usize::try_from((scanline as isize) + output_offset).unwrap();
        let renderer = self.renderer.as_mut().unwrap();
        for (x, pixel) in line.into_iter().enumerate() {
            renderer.set_pixel(x, output_line, pixel);
        }
    }
}

impl<TRenderer> Tickable for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn tick(&mut self, ticks: Ticks) -> Result<()> {
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

                    // Send frame to the screen
                    let renderer = self.renderer.as_mut().unwrap();
                    renderer.update()?;
                }
            } else {
                self.vblank = false;

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

        Ok(())
    }
}

impl<TRenderer> BusMember<Address> for PPU<TRenderer>
where
    TRenderer: Renderer,
{
    fn read(&self, fulladdr: Address) -> Option<u8> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            // SLHV - Latch H/V-Counter by Software (R)
            0x2137 => {
                self.hlatch.set(self.get_current_h() as u8);
                self.vlatch.set(self.get_current_scanline() as u8);

                // Read openbus
                None
            }
            // OPHCT - Horizontal Counter Latch (R)
            0x213C => Some(self.hlatch.get()),
            // OPVCT - Vertical Counter Latch (R)
            0x213D => Some(self.vlatch.get()),
            _ => self.state.read(fulladdr),
        }
    }

    fn write(&mut self, fulladdr: Address, val: u8) -> Option<()> {
        let (_bank, addr) = ((fulladdr >> 16) as usize, (fulladdr & 0xFFFF) as usize);

        match addr {
            _ => self.state.write(fulladdr, val),
        }
    }
}
