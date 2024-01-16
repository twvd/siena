use std::fs::File;
use std::sync::atomic::Ordering;

use anyhow::Result;

use super::DisplayBuffer;

pub struct Gif {
    encoder: gif::Encoder<File>,
    width: usize,
    height: usize,
    delay: u16,
}

impl Gif {
    pub fn new(width: usize, height: usize, _fps: u64, out: File) -> Result<Self> {
        let mut encoder = gif::Encoder::new(out, width.try_into()?, height.try_into()?, &[])?;
        encoder.set_repeat(gif::Repeat::Infinite)?;

        Ok(Self {
            encoder,
            width,
            height,
            delay: 2,
        })
    }

    pub fn add(&mut self, frame: &DisplayBuffer) -> Result<()> {
        let mut rframe: Vec<u8> = frame
            .chunks_exact(4)
            .flat_map(|a| {
                [
                    a[2].load(Ordering::Acquire),
                    a[1].load(Ordering::Acquire),
                    a[0].load(Ordering::Acquire),
                    //a[3].load(Ordering::Relaxed),
                    0,
                ]
            })
            .collect();
        let mut gframe =
            gif::Frame::from_rgba(self.width.try_into()?, self.height.try_into()?, &mut rframe);
        gframe.delay = self.delay;
        Ok(self.encoder.write_frame(&gframe)?)
    }
}
