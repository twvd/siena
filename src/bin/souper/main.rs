use std::fs;
use std::io::{stdin, Read};

use anyhow::Result;
use clap::Parser;

use souper::frontend::sdl::SDLRenderer;
use souper::frontend::Renderer;
use souper::snes::bus::mainbus::{BusTrace, Mainbus};
use souper::snes::bus::Bus;
use souper::snes::cpu_65816::cpu::Cpu65816;
use souper::snes::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

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
    let args = Args::parse();

    let f = fs::read(args.filename)?;
    let load_offset = match f.len() % 1024 {
        0 => 0,
        0x200 => {
            println!("Cartridge contains 0x200 bytes of weird header");
            0x200
        }
        _ => panic!("Illogical cartridge file size: 0x{:08X}", f.len()),
    };

    let display = SDLRenderer::new(SCREEN_WIDTH, SCREEN_HEIGHT)?;
    let bus = Mainbus::<SDLRenderer>::new(&f[load_offset..], args.bustrace, display);

    let reset = bus.read16(0xFFFC);
    let mut cpu = Cpu65816::<Mainbus<SDLRenderer>>::new(bus, reset);

    loop {
        if args.verbose {
            println!("{}", cpu.dump_state());
        }

        if args.pause {
            let _ = stdin().read(&mut [0u8]).unwrap();
        }

        cpu.step()?;
    }
}
