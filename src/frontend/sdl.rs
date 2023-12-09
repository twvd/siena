use std::cell::RefCell;
use std::thread::sleep;
use std::time::{Duration, Instant};

use anyhow::{anyhow, Result};
use sdl2::event::Event;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Canvas, Texture};
use sdl2::video::Window;
use sdl2::{EventPump, Sdl};

use super::{Color, Renderer};

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
    displaybuffer: Vec<u8>,
    width: usize,
    #[allow(dead_code)]
    height: usize,
    last_frame: Instant,
    frametime: u64,

    fps_count: u64,
    fps_time: Instant,
}

impl SDLRenderer {
    const BPP: usize = 4;
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
            let texture_creator = canvas.texture_creator();
            let texture = texture_creator.create_texture_streaming(
                PixelFormatEnum::RGB888,
                width.try_into()?,
                height.try_into()?,
            )?;

            Ok(SDLRenderer {
                canvas,
                texture,
                displaybuffer: vec![0; width * height * Self::BPP],
                width,
                height,
                last_frame: Instant::now(),
                frametime: 1000000 / 50,
                fps_count: 0,
                fps_time: Instant::now(),
            })
        })
    }

    /// Updates a sungle pixel in the backbuffer
    fn set_pixel(&mut self, x: usize, y: usize, color: Color) {
        debug_assert!(x < self.width);
        debug_assert!(y < self.height);

        let idx = ((y * self.width) + x) * Self::BPP;
        self.displaybuffer[idx] = color.2;
        self.displaybuffer[idx + 1] = color.1;
        self.displaybuffer[idx + 2] = color.0;
        // idx + 3 unused. We use the 32-bit version rather than
        // the 24-bit version because it allows for more
        // optimizations in terms of alignment etc.
    }

    /// Renders changes to screen
    fn update(&mut self) -> Result<()> {
        self.texture
            .update(None, &self.displaybuffer, self.width * Self::BPP)?;
        self.canvas
            .copy(&self.texture, None, None)
            .map_err(|e| anyhow!(e))?;
        self.canvas.present();

        self.fps_count += 1;

        if self.fps_time.elapsed().as_secs() >= 2 {
            println!(
                "Frame rate: {:0.2} frames/second",
                self.fps_count as f32 / self.fps_time.elapsed().as_secs_f32()
            );
            self.fps_count = 0;
            self.fps_time = Instant::now();
        }

        // Limit the framerate
        let framelen = self.last_frame.elapsed().as_micros() as u64;
        if framelen < self.frametime {
            sleep(Duration::from_micros(self.frametime - framelen));
        }
        self.last_frame = Instant::now();

        Ok(())
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
