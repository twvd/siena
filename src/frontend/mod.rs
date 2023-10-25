pub mod sdl;

use anyhow::Result;

/// RGB888 format
type Color = (u8, u8, u8);

pub trait Renderer {
    /// Creates a new renderer with a screen of the given size
    fn new(width: usize, height: usize) -> Result<Self>
    where
        Self: Renderer + Sized;

    /// Updates a sungle pixel in the backbuffer
    fn set_pixel(&mut self, x: usize, y: usize, color: Color);

    /// Renders changes to screen
    fn update(&mut self) -> Result<()>;

    /// TODO move to input component
    fn poll(&mut self) -> bool;
}

pub struct NullRenderer {}
impl Renderer for NullRenderer {
    fn new(_width: usize, _height: usize) -> Result<Self> {
        Ok(Self {})
    }

    fn set_pixel(&mut self, _x: usize, _y: usize, _color: Color) {}

    fn update(&mut self) -> Result<()> {
        Ok(())
    }

    fn poll(&mut self) -> bool {
        true
    }
}
