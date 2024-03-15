use std::sync::{Arc, Mutex};

use crate::bus::Bus;
use crate::cpu_65816::cpu::Cpu65816;
use crate::frontend::channel::ChannelRenderer;
#[cfg(not(feature = "apu_blargg"))]
use crate::snes::apu::apu::Apu;
#[cfg(feature = "apu_blargg")]
use crate::snes::apu_blargg::Apu;
use crate::snes::bus::mainbus::{BusTrace, Mainbus};
use crate::snes::cartridge::{Cartridge, VideoFormat};
use crate::snes::joypad::{Joypad, JoypadEventSender, JOYPAD_COUNT};

use anyhow::{anyhow, Result};
use serde::Deserialize;
use serde_json::Deserializer;

pub struct Emulator {
    cpu: Cpu65816<Mainbus<ChannelRenderer>>,
    joypad_senders: Option<[JoypadEventSender; JOYPAD_COUNT]>,
    cpu_verbose: bool,
}

impl Emulator {
    pub fn new(
        cartridge: Cartridge,
        apu_ipl: &[u8],
        displaychannel: ChannelRenderer,
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
        let bus = Mainbus::<ChannelRenderer>::new(
            cartridge,
            BusTrace::None,
            displaychannel,
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
        let cpu = Cpu65816::<Mainbus<ChannelRenderer>>::new(bus, reset);

        Ok(Self {
            cpu,
            joypad_senders: Some(joypad_senders),
            cpu_verbose: false,
        })
    }

    pub fn load_state(&mut self, json: &str) -> Result<()> {
        let mut deserializer = Deserializer::from_str(json);

        // TODO Pending https://github.com/serde-rs/serde/issues/2512
        //Deserialize::deserialize_in_place(&mut deserializer, &mut cpu)?;

        // Until then..
        let mut new_cpu: Cpu65816<Mainbus<ChannelRenderer>> =
            Deserialize::deserialize(&mut deserializer)?;
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
        self.cpu.step()?;

        Ok(())
    }
}
