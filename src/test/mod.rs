pub mod peterlemon_65816;
pub mod peterlemon_bank;
pub mod peterlemon_gsu;
pub mod peterlemon_ppu;
pub mod processortests_65816;
pub mod processortests_spc700;

use itertools::Itertools;
use std::time::Instant;

use crate::frontend::test::TestRenderer;
use crate::snes::cartridge::{Cartridge, Mapper, VideoFormat};
use crate::snes::emulator::Emulator;
use crate::snes::ppu::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

fn test_display(rom: &[u8], pass_hash: &[u8], time_limit: u128, stable: bool, mapper: Mapper) {
    let (display, dispstatus) = TestRenderer::new_test(SCREEN_WIDTH, SCREEN_HEIGHT);
    let cart = Cartridge::load_nohdr(rom, mapper).unwrap();
    let mut emu =
        Emulator::<TestRenderer>::new(cart, &[0; 64], display, Some(VideoFormat::PAL)).unwrap();
    emu.testmode();

    let start = Instant::now();
    loop {
        if start.elapsed().as_millis() > time_limit {
            dbg!(dispstatus.get());
            panic!("Timeout");
        }
        emu.tick().unwrap();

        let newstatus = dispstatus.get();
        if !stable && newstatus.hash == pass_hash {
            return;
        }
        if stable && newstatus.stable_frames >= 20 {
            if newstatus.all_black {
                panic!("Frame is all black");
            }
            if newstatus.hash != pass_hash {
                println!(
                    ":%s/{:02x}/{:02x}",
                    pass_hash.iter().format(""),
                    newstatus.hash.iter().format("")
                );

                panic!(
                    "Expected hash {:02x} but saw {:02x} (for {} frames)",
                    pass_hash.iter().format(""),
                    newstatus.hash.iter().format(""),
                    newstatus.stable_frames
                );
            } else {
                return;
            }
        }
    }
}
