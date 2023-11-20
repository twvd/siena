use std::env;
use std::fs;

use anyhow::{bail, Result};

use siena::snes::cpu_spc700::instruction::Instruction;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        bail!("Syntax: {} <filename>", args[0]);
    }

    let f = fs::read(&args[1])?;
    let mut fiter = f.into_iter();
    let mut pos = 0;
    while let Ok(ins) = Instruction::decode(&mut fiter) {
        println!("{:04X} {}", pos, ins);
        pos += ins.def.len;
    }
    Ok(())
}
