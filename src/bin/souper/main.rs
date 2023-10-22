use std::env;
use std::fs;
use std::io::{stdin, Read};

use anyhow::{bail, Result};

use souper::snes::bus::testbus::Testbus;
use souper::snes::bus::BusMember;
use souper::snes::cpu_65816::cpu::Cpu65816;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        bail!("Syntax: {} <filename>", args[0]);
    }

    let f = fs::read(&args[1])?;

    let mut bus = Testbus::new();

    bus.write_slice(&f[0x200..], 0x8000);
    let reset = bus.read16(0xFFFC);

    let mut cpu = Cpu65816::<Testbus>::new(bus, reset);

    loop {
        println!("{}", cpu.dump_state());

        let _ = stdin().read(&mut [0u8]).unwrap();

        cpu.step()?;
    }
}
