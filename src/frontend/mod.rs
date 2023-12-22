pub mod sdl;
pub mod test;

use anyhow::Result;

use std::sync::{Arc, Mutex};

/// RGB888 format
pub type Color = (u8, u8, u8);

pub trait Renderer {
    /// Creates a new renderer with a screen of the given size
    fn new(width: usize, height: usize) -> Result<Self>
    where
        Self: Renderer + Sized;

    /// Renders changes to screen
    fn update(&mut self) -> Result<()>;

    /// Gets a reference to the (lockable) back buffer
    fn get_buffer(&mut self) -> Arc<Mutex<Vec<u8>>>;
}

pub struct NullRenderer {}
impl Renderer for NullRenderer {
    fn new(_width: usize, _height: usize) -> Result<Self> {
        Ok(Self {})
    }

    fn update(&mut self) -> Result<()> {
        Ok(())
    }

    fn get_buffer(&mut self) -> Arc<Mutex<Vec<u8>>> {
        unreachable!()
    }
}
