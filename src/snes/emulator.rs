use std::cmp::min;
use std::sync::{Arc, Mutex};

use crate::bus::Bus;
use crate::cpu_65816::cpu::Cpu65816;
use crate::frontend::Renderer;
#[cfg(not(feature = "apu_blargg"))]
use crate::snes::apu::apu::Apu;
#[cfg(feature = "apu_blargg")]
use crate::snes::apu_blargg::Apu;
use crate::snes::bus::mainbus::{BusTrace, Mainbus};
use crate::snes::cartridge::{Cartridge, VideoFormat};
use crate::snes::joypad::{Joypad, JoypadEventSender, JOYPAD_COUNT};
use crate::tickable::{Tickable, Ticks};

use anyhow::{anyhow, Result};
use enum_map::{Enum, EnumMap};
use serde::Deserialize;
use serde_json::Deserializer;
use strum::{EnumIter, IntoEnumIterator};

#[derive(Debug, Eq, PartialEq, Enum, EnumIter, Clone, Copy)]
pub enum Schedule {
    SCPU = 0,
    PPU = 1,
    SPC700 = 2,
    DSP1 = 3,
    SuperFX = 4,
}

pub struct Emulator<T>
where
    T: Renderer,
{
    cpu: Cpu65816<Mainbus<T>>,
    joypad_senders: Option<[JoypadEventSender; JOYPAD_COUNT]>,
    cpu_verbose: bool,
    schedule_next: EnumMap<Schedule, Ticks>,
    schedule_ticks: Ticks,
}

impl<T> Emulator<T>
where
    T: Renderer,
{
    pub fn new(
        cartridge: Cartridge,
        apu_ipl: &[u8],
        renderer: T,
        ovr_videoformat: Option<VideoFormat>,
    ) -> Result<Self> {
        // Set up joypad inputs
        let (joypads, joypad_senders) = Joypad::new_channel_all();

        // Determine video format (PAL/NTSC)
        let videoformat = ovr_videoformat.unwrap_or(cartridge.get_video_format());

        // Determine frame rate limit based on the video format
        let fps = match videoformat {
            VideoFormat::NTSC => 60,
            VideoFormat::PAL => 50,
        };

        // Initialize S-CPU bus
        let bus = Mainbus::<T>::new(
            cartridge,
            BusTrace::None,
            renderer,
            joypads,
            apu_ipl,
            false,
            fps,
            videoformat,
        );

        // Fetch reset vector address
        let reset = bus.read16(0xFFFC);
        println!(
            "Reset at PC {:06X}, NMI at {:06X}, IRQ at {:06X}",
            reset,
            bus.read16(0xFFEA),
            bus.read16(0xFFEE)
        );

        // Initialize S-CPU
        let cpu = Cpu65816::<Mainbus<T>>::new(bus, reset);

        let mut emu = Self {
            cpu,
            joypad_senders: Some(joypad_senders),
            cpu_verbose: false,
            schedule_next: EnumMap::default(),
            schedule_ticks: 0,
        };

        // Initialize scheduling for co-processors
        if emu.cpu.bus.cartridge.co_dsp1.is_none() {
            emu.schedule_next[Schedule::DSP1] = Ticks::MAX;
        }
        if emu.cpu.bus.cartridge.co_superfx.is_none() {
            emu.schedule_next[Schedule::SuperFX] = Ticks::MAX;
        }

        Ok(emu)
    }

    pub fn testmode(&mut self) {
        self.cpu.bus.ppu.single_threaded();
        self.schedule_next[Schedule::SPC700] = Ticks::MAX;
        self.set_fps_limit(0);
    }

    pub fn load_state(&mut self, json: &str) -> Result<()> {
        let mut deserializer = Deserializer::from_str(json);

        // TODO Pending https://github.com/serde-rs/serde/issues/2512
        //Deserialize::deserialize_in_place(&mut deserializer, &mut cpu)?;

        // Until then..
        let mut new_cpu: Cpu65816<Mainbus<T>> = Deserialize::deserialize(&mut deserializer)?;
        // ..and move all the non-serializable stuff over.
        new_cpu.bus.ppu.renderer = std::mem::replace(&mut self.cpu.bus.ppu.renderer, None);

        new_cpu.bus.joypads = std::mem::replace(&mut self.cpu.bus.joypads, None);

        self.cpu = new_cpu;
        Ok(())
    }

    pub fn dump_state(&self, writer: impl std::io::Write) -> Result<()> {
        serde_json::to_writer(writer, &self.cpu)?;
        Ok(())
    }

    pub fn set_joypad_sticky(&mut self, v: bool) {
        for j in self.cpu.bus.joypads.as_mut().unwrap().iter_mut() {
            j.sticky_enabled = v;
        }
    }

    pub fn set_fps_limit(&mut self, new_limit: u64) {
        self.cpu.bus.ppu.set_fps_limit(new_limit);
    }

    pub fn set_trace_bus(&mut self, v: BusTrace) {
        self.cpu.bus.trace = v;
    }

    pub fn set_trace_apu(&mut self, v: bool) {
        #[cfg(not(feature = "apu_blargg"))]
        {
            let mut apu = self.cpu.bus.apu.lock().unwrap();
            apu.verbose = v;
        }
    }

    pub fn set_trace_apu_comm(&mut self, v: bool) {
        #[cfg(not(feature = "apu_blargg"))]
        {
            let apu = self.cpu.bus.apu.lock().unwrap();
            apu.ports.write().unwrap().trace = v;
        }
    }

    pub fn get_apu(&mut self) -> Arc<Mutex<Apu>> {
        self.cpu.bus.get_apu()
    }

    pub fn set_verbose_cpu(&mut self, v: bool) {
        self.cpu_verbose = v;
    }

    pub fn toggle_verbose_cpu(&mut self) {
        self.cpu_verbose = !self.cpu_verbose;
    }

    pub fn set_verbose_spc(&mut self, v: bool) {
        #[cfg(not(feature = "apu_blargg"))]
        {
            let mut apu = self.cpu.bus.apu.lock().unwrap();
            apu.verbose = v;
        }
    }

    pub fn toggle_verbose_spc(&mut self) {
        #[cfg(not(feature = "apu_blargg"))]
        {
            let mut apu = self.cpu.bus.apu.lock().unwrap();
            apu.verbose = !apu.verbose;
        }
    }

    pub fn toggle_verbose_gsu(&mut self) {
        if let Some(sfx) = self.cpu.bus.cartridge.co_superfx.as_mut() {
            let mut gsu = sfx.cpu.borrow_mut();
            gsu.verbose = !gsu.verbose;
        }
    }

    pub fn get_joypad_senders(&mut self) -> Result<[JoypadEventSender; JOYPAD_COUNT]> {
        self.joypad_senders
            .take()
            .ok_or(anyhow!("Joypad senders taken twice"))
    }

    pub fn tick(&mut self) -> Result<()> {
        // The scheduler assumes a base clock, which is the SNES master clock,
        // monotonically increasing. This is not based on time; the wall clock time
        // speed of the emulator is regulated by the frame rate limiter.
        //
        // Every cycle-based component receives time to do a step when it
        // needs to run (based on a divider from the master clock).
        //
        // When a step is done, the component returns the amount of cycles
        // actually used. The next time the component is stepped is then
        // delayed taking into account the used cycles last time and the
        // clock divider.

        let mut inc_ticks = usize::MAX;
        for comp in Schedule::iter() {
            if self.schedule_next[comp] <= self.schedule_ticks {
                // This component can take one step now
                let cycles_spent = self.scheduler_dispatch(comp)?;
                //println!("{:?}: {} ticks", comp, cycles_spent);

                self.schedule_next[comp] = self.schedule_ticks + cycles_spent;
                inc_ticks = min(inc_ticks, cycles_spent);
            }
        }

        self.schedule_ticks += inc_ticks;

        Ok(())
    }

    fn scheduler_dispatch(&mut self, component: Schedule) -> Result<Ticks> {
        // Base frequency: ~21 MHz
        match component {
            Schedule::SCPU => {
                // 3.5 MHz (no wait states)
                if self.cpu_verbose {
                    println!("{}", self.cpu.dump_state());
                }
                Ok(self.cpu.tick(1)? * 6)
            }
            Schedule::PPU => {
                // 5.3 MHz
                Ok(self.cpu.bus.ppu.tick(1)? * 4)
            }
            Schedule::SPC700 => {
                // 1.024 MHz
                let mut apu = self.cpu.bus.apu.lock().unwrap();
                Ok(apu.tick(1)? * 21)
            }
            Schedule::SuperFX => {
                // 20 MHz (or 10, then CPU will double cycles)
                Ok(self
                    .cpu
                    .bus
                    .cartridge
                    .co_superfx
                    .as_mut()
                    .unwrap()
                    .tick(1)?
                    * 1)
            }
            Schedule::DSP1 => {
                // 7 MHz
                Ok(self.cpu.bus.cartridge.co_dsp1.as_mut().unwrap().tick(1)? * 3)
            }
        }
    }
}
