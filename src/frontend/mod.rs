pub mod sdl;

use anyhow::Result;

pub trait Renderer {
    fn new() -> Result<Self>
    where
        Self: Renderer + Sized;
    fn set_pixel(&mut self, x: usize, y: usize, r: usize, g: usize, b: usize);
    fn poll(&mut self) -> bool;
}
