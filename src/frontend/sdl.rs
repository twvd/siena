use std::cell::RefCell;

use anyhow::{anyhow, Result};
use sdl2::event::Event;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Canvas, Texture, TextureCreator};
use sdl2::video::{Window, WindowContext};
use sdl2::{EventPump, Sdl};

use super::Renderer;

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
}

impl Renderer for SDLRenderer {
    fn new() -> Result<Self> {
        SDL.with(|cell| {
            let sdls = cell.borrow_mut();
            let video_subsystem = sdls.context.video().map_err(|e| anyhow!(e))?;
            let window = video_subsystem
                .window("Souper", 800, 600)
                .position_centered()
                .build()?;

            let canvas = window.into_canvas().build()?;
            let texture_creator = canvas.texture_creator();
            let texture =
                texture_creator.create_texture_target(PixelFormatEnum::RGB888, 512, 448)?;

            Ok(SDLRenderer { canvas, texture })
        })
    }

    fn set_pixel(&mut self, x: usize, y: usize, r: usize, g: usize, b: usize) {}

    fn poll(&mut self) -> bool {
        SDL.with(|cell| {
            let mut sdls = cell.borrow_mut();
            for ev in sdls.pump.poll_iter() {
                match ev {
                    Event::Quit { .. } => return false,
                    _ => (),
                }
            }

            true
        })
    }
}
