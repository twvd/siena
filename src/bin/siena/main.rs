use std::fs;
use std::io::{stdin, Read};

use anyhow::Result;
use clap::Parser;
use colored::*;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use siena::frontend::sdl::{SDLEventPump, SDLRenderer};
use siena::frontend::Renderer;
use siena::snes::bus::mainbus::{BusTrace, Mainbus};
use siena::snes::bus::Bus;
use siena::snes::cartridge::Cartridge;
use siena::snes::cpu_65816::cpu::Cpu65816;
use siena::snes::joypad::{Button, Joypad, JoypadEvent};
use siena::snes::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

fn map_keycode(keycode: Keycode) -> Option<(usize, Button)> {
    match keycode {
        Keycode::A => Some((0, Button::A)),
        Keycode::B => Some((0, Button::B)),
        Keycode::X => Some((0, Button::X)),
        Keycode::Y => Some((0, Button::Y)),
        Keycode::Space => Some((0, Button::Start)),
        Keycode::Period => Some((0, Button::Select)),
        Keycode::Up => Some((0, Button::Up)),
        Keycode::Down => Some((0, Button::Down)),
        Keycode::Left => Some((0, Button::Left)),
        Keycode::Right => Some((0, Button::Right)),

        _ => None,
    }
}

#[derive(Parser)]
#[command(
    about = "SNES Emulator",
    author = "Thomas <thomas@thomasw.dev>",
    long_about = None)]
struct Args {
    /// ROM filename to load.
    filename: String,

    /// Wait for keystroke after each CPU step.
    #[arg(short, long)]
    pause: bool,

    /// Print CPU state after each instruction
    #[arg(short, long)]
    verbose: bool,

    /// Sticky joypad controls
    #[arg(long)]
    sticky: bool,

    /// Bus trace mode
    #[arg(
        long,
        require_equals = true,
        value_name = "MODE",
        num_args = 0..=1,
        default_value_t = BusTrace::None,
        default_missing_value = "none",
        value_enum
    )]
    bustrace: BusTrace,
}

fn main() -> Result<()> {
    let mut args = Args::parse();

    let f = fs::read(args.filename)?;

    let (mut joypads, joypad_senders) = Joypad::new_channel_all();
    for j in joypads.iter_mut() {
        j.sticky_enabled = args.sticky;
    }
    let display = SDLRenderer::new(SCREEN_WIDTH, SCREEN_HEIGHT)?;
    let eventpump = SDLEventPump::new();
    let cart = Cartridge::load(&f);
    println!("Cartridge: {}", &cart);
    let bus = Mainbus::<SDLRenderer>::new(cart, args.bustrace, display, joypads, args.verbose);

    let reset = bus.read16(0xFFFC);
    let mut cpu = Cpu65816::<Mainbus<SDLRenderer>>::new(bus, reset);

    let mut eventpoll = 0;
    'mainloop: loop {
        if args.verbose {
            println!("{}", cpu.dump_state().green());
        }

        if args.pause {
            let _ = stdin().read(&mut [0u8]).unwrap();
        }

        cpu.step()?;

        eventpoll += 1;
        // Polling SDL events is too expensive to do every step..
        // TODO do this once per VBlank or something..
        if eventpoll == 1000 {
            eventpoll = 0;
            while let Some(event) = eventpump.poll() {
                match event {
                    // Application exit
                    Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    }
                    | Event::Quit { .. } => break 'mainloop,

                    // Debug - toggle verbose (instruction trace)
                    Event::KeyDown {
                        keycode: Some(Keycode::V),
                        ..
                    } => args.verbose = !args.verbose,

                    // Debug - toggle open bus trace
                    Event::KeyDown {
                        keycode: Some(Keycode::C),
                        ..
                    } => {
                        cpu.bus.trace = match cpu.bus.trace {
                            BusTrace::None => BusTrace::Open,
                            _ => BusTrace::None,
                        }
                    }

                    // Debug - toggle layer masks
                    Event::KeyDown {
                        keycode: Some(Keycode::Num1),
                        ..
                    } => cpu.bus.ppu.dbg_layermask ^= 1 << 0,
                    Event::KeyDown {
                        keycode: Some(Keycode::Num2),
                        ..
                    } => cpu.bus.ppu.dbg_layermask ^= 1 << 1,
                    Event::KeyDown {
                        keycode: Some(Keycode::Num3),
                        ..
                    } => cpu.bus.ppu.dbg_layermask ^= 1 << 2,
                    Event::KeyDown {
                        keycode: Some(Keycode::Num4),
                        ..
                    } => cpu.bus.ppu.dbg_layermask ^= 1 << 3,
                    Event::KeyDown {
                        keycode: Some(Keycode::Num5),
                        ..
                    } => cpu.bus.ppu.dbg_layermask ^= 1 << 4,

                    // Controller input
                    Event::KeyDown {
                        keycode: Some(k), ..
                    } => {
                        if let Some((padidx, button)) = map_keycode(k) {
                            joypad_senders[padidx].send(JoypadEvent::Down(button))?;
                        }
                    }
                    Event::KeyUp {
                        keycode: Some(k), ..
                    } => {
                        if let Some((padidx, button)) = map_keycode(k) {
                            joypad_senders[padidx].send(JoypadEvent::Up(button))?;
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    Ok(())
}
