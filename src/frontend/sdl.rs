use std::cell::RefCell;
use std::sync::atomic::AtomicU8;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use anyhow::{anyhow, Result};
use sdl2::audio::AudioCallback;
use sdl2::audio::AudioDevice;
use sdl2::audio::AudioSpecDesired;
use sdl2::event::Event;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Canvas, Texture};
use sdl2::video::Window;
use sdl2::{EventPump, Sdl};

use super::{new_displaybuffer, DisplayBuffer, Renderer};

use crate::snes::apu_blargg::Apu;

pub struct SDLSingleton {
    context: Sdl,
    pump: EventPump,
}

thread_local! {
    static SDL: RefCell<SDLSingleton> = RefCell::new({
        let context = sdl2::init().unwrap();
        let pump = context.event_pump().unwrap();

        SDLSingleton {
            context,
            pump
        }
    });
}

pub struct SDLRenderer {
    canvas: Canvas<Window>,
    texture: Texture,
    displaybuffer: DisplayBuffer,
    width: usize,
    #[allow(dead_code)]
    height: usize,

    fps_count: u64,
    fps_time: Instant,
}

impl SDLRenderer {
    const BPP: usize = 4;

    pub fn update_from(&mut self, buffer: DisplayBuffer) -> Result<()> {
        // This is safe because SDL will only read from the transmuted
        // buffer. Worst case is a garbled display.
        let sdl_displaybuffer = unsafe { std::mem::transmute::<&[AtomicU8], &[u8]>(&buffer) };
        self.texture
            .update(None, &sdl_displaybuffer, self.width * Self::BPP)?;
        self.canvas.clear();
        self.canvas
            .copy(&self.texture, None, None)
            .map_err(|e| anyhow!(e))?;
        self.canvas.present();

        self.fps_count += 1;

        if self.fps_time.elapsed().as_secs() >= 2 {
            println!(
                "SDL Frame rate: {:0.2} frames/second",
                self.fps_count as f32 / self.fps_time.elapsed().as_secs_f32()
            );
            self.fps_count = 0;
            self.fps_time = Instant::now();
        }

        Ok(())
    }
}

impl Renderer for SDLRenderer {
    /// Creates a new renderer with a screen of the given size
    fn new(width: usize, height: usize) -> Result<Self> {
        SDL.with(|cell| {
            let sdls = cell.borrow_mut();
            let video_subsystem = sdls.context.video().map_err(|e| anyhow!(e))?;
            let window = video_subsystem
                .window(
                    "Siena SNES emulator",
                    (width * 3).try_into()?,
                    (height * 3).try_into()?,
                )
                .position_centered()
                .build()?;

            let canvas = window.into_canvas().accelerated().build()?;
            println!("Rendering driver: {:?}", canvas.info().name);
            let texture_creator = canvas.texture_creator();
            let texture = texture_creator.create_texture_streaming(
                PixelFormatEnum::RGB888,
                width.try_into()?,
                height.try_into()?,
            )?;

            Ok(SDLRenderer {
                canvas,
                texture,
                displaybuffer: new_displaybuffer(width, height),
                width,
                height,
                fps_count: 0,
                fps_time: Instant::now(),
            })
        })
    }

    fn get_buffer(&mut self) -> DisplayBuffer {
        Arc::clone(&self.displaybuffer)
    }

    /// Renders changes to screen
    fn update(&mut self) -> Result<()> {
        self.update_from(Arc::clone(&self.displaybuffer))
    }
}

pub struct SDLEventPump {}
impl SDLEventPump {
    pub fn new() -> Self {
        Self {}
    }

    pub fn poll(&self) -> Option<Event> {
        SDL.with(|cell| {
            let mut sdls = cell.borrow_mut();
            sdls.pump.poll_event()
        })
    }
}

pub struct SDLAudioSink {
    apu: Arc<Mutex<Apu>>,
}

impl AudioCallback for SDLAudioSink {
    type Channel = i16;

    fn callback(&mut self, out: &mut [i16]) {
        let mut apu = self.apu.lock().unwrap();
        apu.render(out);
    }
}

impl SDLAudioSink {
    /// Creates a new audiosink
    pub fn init(apu: Arc<Mutex<Apu>>) -> Result<AudioDevice<SDLAudioSink>> {
        SDL.with(|cell| {
            let sdls = cell.borrow_mut();
            let audio_subsystem = sdls.context.audio().map_err(|e| anyhow!(e))?;
            let spec = AudioSpecDesired {
                freq: Some(32000),
                channels: Some(2),
                samples: Some(128),
            };

            let device = audio_subsystem
                .open_playback(None, &spec, |spec| {
                    dbg!(&spec);
                    Self { apu }
                })
                .map_err(|e| anyhow!(e))?;
            device.resume();
            Ok(device)
        })
    }
}
