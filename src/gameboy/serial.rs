use anyhow::Result;
use std::io;
use std::io::Write;

use crate::gameboy::bus::bus::BusMember;
use crate::gameboy::tickable::{Tickable, Ticks};

/// Serial (link cable) controller
pub struct Serial {
    /// Serial data buffer
    serialbuffer: u8,

    /// Serial Control register
    sc: u8,

    /// Serial input stream
    serial_in: Option<Box<dyn io::Read>>,

    /// Serial output stream
    serial_out: Option<Box<dyn io::Write>>,

    /// Interrupt request
    intreq: bool,
}

impl Serial {
    pub fn new_null() -> Self {
        Self::_new(None, None)
    }

    pub fn new_out(serial_out: Box<dyn io::Write>) -> Self {
        Self::_new(None, Some(serial_out))
    }

    pub fn new(serial_in: Box<dyn io::Read>, serial_out: Box<dyn io::Write>) -> Self {
        Self::_new(Some(serial_in), Some(serial_out))
    }

    fn _new(serial_in: Option<Box<dyn io::Read>>, serial_out: Option<Box<dyn io::Write>>) -> Self {
        Self {
            serial_in,
            serial_out,
            serialbuffer: 0,
            sc: 0,
            intreq: false,
        }
    }

    pub fn get_clr_intreq(&mut self) -> bool {
        let val = self.intreq;
        self.intreq = false;
        val
    }
}

impl BusMember for Serial {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // I/O - Serial transfer data buffer
            0xFF01 => self.serialbuffer,

            // I/O - Serial transfer control
            0xFF02 => self.sc,

            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // I/O - Serial transfer data buffer
            0xFF01 => self.serialbuffer = val,

            // I/O - Serial transfer control
            0xFF02 => {
                if val & 0x81 == 0x81 {
                    if let Some(ref mut so) = &mut self.serial_out {
                        so.write(&[self.serialbuffer]).unwrap();
                    }
                    if let Some(ref mut si) = &mut self.serial_in {
                        let mut buf = [0; 1];
                        loop {
                            match si.read_exact(&mut buf) {
                                Ok(()) => break,
                                _ => (),
                            }
                        }
                        self.serialbuffer = buf[0];
                    } else {
                        self.serialbuffer = 0xFF;
                    }
                }
                self.sc = val & !0x80;
            }

            _ => unreachable!(),
        }
    }
}

impl Tickable for Serial {
    fn tick(&mut self, _ticks: Ticks) -> Result<()> {
        if let Some(ref mut si) = &mut self.serial_in {
            let mut buf = [0; 1];
            match si.read_exact(&mut buf) {
                Ok(()) => {
                    if let Some(ref mut so) = &mut self.serial_out {
                        so.write(&[self.serialbuffer]).unwrap();
                    }
                    self.serialbuffer = buf[0];
                    self.sc = 0x01;
                    self.intreq = true;
                }
                _ => (),
            }
        }

        Ok(())
    }
}
