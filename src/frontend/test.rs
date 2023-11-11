use super::{Color, Renderer};

use anyhow::Result;
use sha2::{Digest, Sha256};

use std::cell::Cell;
use std::cmp;
use std::rc::Rc;

/// A display that hashes the contents using SHA256.
pub struct TestRenderer {
    width: usize,
    height: usize,
    buffer: Vec<Vec<Color>>,
    state: TDS,
}

#[derive(Debug, Copy, Clone)]
pub struct TestRendererState {
    pub stable_frames: u16,
    pub hash: [u8; 256 / 8],
}

pub type TDS = Rc<Cell<TestRendererState>>;

impl TestRenderer {
    pub fn new_test(width: usize, height: usize) -> (Self, TDS) {
        let mut vs: Vec<Vec<Color>> = Vec::with_capacity(height);
        for _ in 0..height {
            let mut vline = Vec::<Color>::with_capacity(width);
            for _ in 0..width {
                vline.push((0, 0, 0));
            }
            vs.push(vline);
        }

        let state = Rc::new(Cell::new(TestRendererState {
            stable_frames: 0,
            hash: [0; 256 / 8],
        }));

        (
            TestRenderer {
                width,
                height,
                buffer: vs,
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

    fn set_pixel(&mut self, x: usize, y: usize, color: Color) {
        assert!(x < self.width);
        assert!(y < self.height);

        self.buffer[y][x] = color;
    }

    fn update(&mut self) -> Result<()> {
        let mut hasher = Sha256::new();
        hasher.update(
            self.buffer
                .iter()
                .flat_map(|v| v.clone().into_iter())
                .flat_map(|i| [i.0, i.1, i.2].into_iter())
                .collect::<Vec<u8>>(),
        );
        let hash = hasher.finalize();

        let oldstate = self.state.get();
        let stable_frames = if oldstate.hash == hash[..] {
            cmp::min(oldstate.stable_frames + 1, u16::MAX - 1)
        } else {
            1
        };

        self.state.set(TestRendererState {
            hash: hash.into(),
            stable_frames,
        });
        Ok(())
    }
}
