use std::fs;
use std::io::{stdin, Read};
use std::time::SystemTime;

use anyhow::Result;
use clap::Parser;
use colored::*;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use serde::Deserialize;
use serde_json::Deserializer;

use siena::frontend::sdl::{SDLEventPump, SDLRenderer};
use siena::frontend::Renderer;
use siena::snes::bus::mainbus::{BusTrace, Mainbus};
use siena::snes::bus::Bus;
use siena::snes::cartridge::Cartridge;
use siena::snes::cpu_65816::cpu::Cpu65816;
use siena::snes::joypad::{Button, Joypad, JoypadEvent};
use siena::snes::ppu::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

fn map_keycode(keycode: Keycode) -> Option<(usize, Button)> {
    match keycode {
        Keycode::A => Some((0, Button::A)),
        Keycode::B => Some((0, Button::B)),
        Keycode::X => Some((0, Button::X)),
        Keycode::Y => Some((0, Button::Y)),
        Keycode::L => Some((0, Button::L)),
        Keycode::R => Some((0, Button::R)),
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

    /// Print S-CPU state after each instruction
    #[arg(short, long)]
    verbose: bool,

    /// Print SPC700 state after each instruction
    #[arg(long)]
    spc_verbose: bool,

    /// Sticky joypad controls
    #[arg(long)]
    sticky: bool,

    /// S-CPU bus trace mode
    #[arg(
        long,
        require_equals = true,
        value_name = "MODE",
        num_args = 0..=1,
        default_value_t = BusTrace::None,
        default_missing_value = "none",
        value_enum
    )]
    trace_bus: BusTrace,

    /// S-CPU <-> APU communication trace
    #[arg(long)]
    trace_apu_comm: bool,

    /// Load state file
    #[arg(long)]
    state: Option<String>,

    /// Skip cartridge header detection, load as LoROM (mostly for test ROMs)
    #[arg(long)]
    no_header: bool,

    /// Skip cartridge header detection, load as HiROM (mostly for test ROMs)
    #[arg(long)]
    no_header_hirom: bool,
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
    let cart = if !args.no_header && !args.no_header_hirom {
        let c = Cartridge::load(&f);
        println!("Cartridge: {}", &c);
        c
    } else {
        Cartridge::load_nohdr(&f, args.no_header_hirom)
    };
    let mut bus = Mainbus::<SDLRenderer>::new(cart, args.trace_bus, display, joypads, args.verbose);
    bus.apu.verbose = args.spc_verbose;
    bus.apu.ports.borrow_mut().trace = args.trace_apu_comm;

    let reset = bus.read16(0xFFFC);
    println!("Reset at PC {:06X}", reset);
    let mut cpu = Cpu65816::<Mainbus<SDLRenderer>>::new(bus, reset);

    if let Some(state_filename) = args.state {
        // Load and deserialize state file
        println!("Restoring state from {}", state_filename);
        let json = fs::read_to_string(state_filename)?;
        let mut deserializer = Deserializer::from_str(&json);

        // TODO Pending https://github.com/serde-rs/serde/issues/2512
        //Deserialize::deserialize_in_place(&mut deserializer, &mut cpu)?;

        // Until then..
        let mut new_cpu: Cpu65816<Mainbus<SDLRenderer>> =
            Deserialize::deserialize(&mut deserializer)?;
        // ..and move all the non-serializable stuff over.
        new_cpu.bus.ppu.renderer = std::mem::replace(&mut cpu.bus.ppu.renderer, None);
        new_cpu.bus.joypads = std::mem::replace(&mut cpu.bus.joypads, None);

        cpu = new_cpu;
    }

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
                        keycode: Some(Keycode::Num0),
                        ..
                    } => args.verbose = !args.verbose,

                    // Debug - toggle SPC700 verbose (instruction trace)
                    Event::KeyDown {
                        keycode: Some(Keycode::Num9),
                        ..
                    } => cpu.bus.apu.verbose = !cpu.bus.apu.verbose,

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

                    // Dump state
                    Event::KeyDown {
                        keycode: Some(Keycode::D),
                        ..
                    } => {
                        let filename = format!(
                            "state_{}.json",
                            SystemTime::now()
                                .duration_since(SystemTime::UNIX_EPOCH)
                                .expect("Timetravel detected")
                                .as_secs()
                        );
                        let file = fs::File::create(&filename)?;
                        serde_json::to_writer(file, &cpu)?;
                        println!("State dumped to {}", filename);
                    }

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
