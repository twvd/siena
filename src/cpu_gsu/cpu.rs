use serde::{Deserialize, Serialize};

use super::regs::RegisterFile;

use crate::tickable::Ticks;

/// SuperFX CPU (GSU)
#[derive(Serialize, Deserialize)]
pub struct CpuGsu {
    pub regs: RegisterFile,
    pub cycles: Ticks,
}

impl CpuGsu {
    pub fn new() -> Self {
        Self {
            regs: RegisterFile::new(),
            cycles: 0,
        }
    }
}
