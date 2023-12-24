use std::sync::Arc;
use std::time::Instant;

use anyhow::Result;
use crossbeam_channel::{Receiver, Sender, TrySendError};

use super::{new_displaybuffer, DisplayBuffer, Renderer};

/// A renderer that feeds it display buffer back over a channel.
pub struct ChannelRenderer {
    displaybuffer: DisplayBuffer,
    sender: Sender<DisplayBuffer>,
    receiver: Receiver<DisplayBuffer>,
    width: usize,
    height: usize,

    fps_count: u64,
    fps_time: Instant,
}

impl ChannelRenderer {
    pub fn get_receiver(&mut self) -> Receiver<DisplayBuffer> {
        self.receiver.clone()
    }
}

impl Renderer for ChannelRenderer {
    /// Creates a new renderer with a screen of the given size
    fn new(width: usize, height: usize) -> Result<Self> {
        let (sender, receiver) = crossbeam_channel::bounded(1);
        Ok(Self {
            displaybuffer: new_displaybuffer(width, height),
            sender,
            receiver,
            width,
            height,

            fps_count: 0,
            fps_time: Instant::now(),
        })
    }

    fn get_buffer(&mut self) -> DisplayBuffer {
        Arc::clone(&self.displaybuffer)
    }

    /// Renders changes to screen
    fn update(&mut self) -> Result<()> {
        self.fps_count += 1;

        if self.fps_time.elapsed().as_secs() >= 2 {
            println!(
                "PPU Frame rate: {:0.2} frames/second",
                self.fps_count as f32 / self.fps_time.elapsed().as_secs_f32()
            );
            self.fps_count = 0;
            self.fps_time = Instant::now();
        }

        let buffer = std::mem::replace(
            &mut self.displaybuffer,
            new_displaybuffer(self.width, self.height),
        );
        match self.sender.try_send(buffer) {
            Err(TrySendError::Full(_)) => Ok(()),
            e => Ok(e?),
        }
    }
}
