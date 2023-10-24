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
