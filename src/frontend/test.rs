use super::{DisplayBuffer, Renderer};

use anyhow::Result;
use itertools::Itertools;
use sha2::{Digest, Sha256};

use std::cell::Cell;
use std::cmp;
use std::iter;
use std::rc::Rc;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;

/// A display that hashes the contents using SHA256.
pub struct TestRenderer {
    buffer: DisplayBuffer,
    state: TDS,
}

#[derive(Debug, Copy, Clone)]
pub struct TestRendererState {
    pub stable_frames: u16,
    pub hash: [u8; 256 / 8],
    pub all_black: bool,
}

pub type TDS = Rc<Cell<TestRendererState>>;

impl TestRenderer {
    pub fn new_test(width: usize, height: usize) -> (Self, TDS) {
        let state = Rc::new(Cell::new(TestRendererState {
            stable_frames: 0,
            hash: [0; 256 / 8],
            all_black: true,
        }));

        (
            TestRenderer {
                buffer: Arc::new(Vec::from_iter(
                    iter::repeat_with(|| AtomicU8::new(0)).take(width * height * 4),
                )),
                state: Rc::clone(&state),
            },
            state,
        )
    }
}

impl Renderer for TestRenderer {
    /// Creates a new renderer with a screen of the given size
    fn new(_width: usize, _height: usize) -> Result<Self>
    where
        Self: Renderer + Sized,
    {
        panic!("Use new_test().");
    }

    fn update(&mut self) -> Result<()> {
        let mut hasher = Sha256::new();
        hasher.update(
            // Shuffle them around into the old format, BEFORE
            // everything started using 32-bit RGB, which was just
            // 24-bit R, G, B.
            self.buffer
                .iter()
                .map(|a| a.load(Ordering::Acquire))
                .chunks(4)
                .into_iter()
                .flat_map(|i| i.take(3).collect::<Vec<_>>().into_iter().rev())
                .collect::<Vec<u8>>(),
        );
        let hash = hasher.finalize();

        let oldstate = self.state.get();
        let stable_frames = if oldstate.hash == hash[..] {
            cmp::min(oldstate.stable_frames + 1, u16::MAX - 1)
        } else {
            1
        };

        let all_black = self.buffer.iter().all(|c| c.load(Ordering::Acquire) == 0);

        self.state.set(TestRendererState {
            hash: hash.into(),
            stable_frames,
            all_black,
        });
        Ok(())
    }

    fn get_buffer(&mut self) -> DisplayBuffer {
        Arc::clone(&self.buffer)
    }
}
