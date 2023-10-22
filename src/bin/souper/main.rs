use std::env;
use std::fs;
use std::io::{stdin, Read};

use anyhow::{bail, Result};
use clap::Parser;

use souper::snes::bus::mainbus::Mainbus;
use souper::snes::bus::BusMember;
use souper::snes::cpu_65816::cpu::Cpu65816;

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
}

fn main() -> Result<()> {
    let args = Args::parse();

    let f = fs::read(args.filename)?;

    let bus = Mainbus::new(&f);

    let reset = bus.read16(0xFFFC);
    let mut cpu = Cpu65816::<Mainbus>::new(bus, reset);

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
