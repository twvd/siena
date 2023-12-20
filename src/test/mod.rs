pub mod peterlemon_65816;
pub mod peterlemon_bank;
pub mod peterlemon_ppu;
pub mod processortests_65816;
pub mod processortests_spc700;

use itertools::Itertools;
use std::time::Instant;

use crate::frontend::test::TestRenderer;
use crate::snes::bus::mainbus::{BusTrace, Mainbus};
use crate::snes::bus::Bus;
use crate::snes::cartridge::Cartridge;
use crate::snes::cpu_65816::cpu::Cpu65816;
use crate::snes::joypad::Joypad;
use crate::snes::ppu::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

fn test_display(rom: &[u8], pass_hash: &[u8], time_limit: u128, stable: bool, hirom: bool) {
    let (display, dispstatus) = TestRenderer::new_test(SCREEN_WIDTH, SCREEN_HEIGHT);
    let (joypads, _) = Joypad::new_channel_all();
    let cart = Cartridge::load_nohdr(rom, hirom);
    let bus = Mainbus::<TestRenderer>::new(cart, BusTrace::None, display, joypads, false);
    let reset = bus.read16(0xFFFC);
    let mut cpu = Cpu65816::<Mainbus<TestRenderer>>::new(bus, reset);

    let start = Instant::now();
    loop {
        if start.elapsed().as_millis() > time_limit {
            dbg!(dispstatus.get());
            panic!("Timeout");
        }
        cpu.step().unwrap();

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
