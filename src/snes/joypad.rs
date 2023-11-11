use std::cell::Cell;
use std::sync::mpsc;

use num_derive::ToPrimitive;
use num_traits::ToPrimitive;
use strum::EnumCount;

pub const JOYPAD_COUNT: usize = 4;

pub type Joypads = [Joypad; JOYPAD_COUNT];
pub type JoypadEventSender = mpsc::Sender<JoypadEvent>;

#[derive(Debug, ToPrimitive, EnumCount)]
pub enum Button {
    B,
    Y,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
    A,
    X,
    L,
    R,
}

#[derive(Debug)]
pub enum JoypadEvent {
    Down(Button),
    Up(Button),
}

#[derive(Debug)]
pub struct Joypad {
    pub state: Cell<u16>,
    pub scan_pos: Cell<u8>,
    event_recv: mpsc::Receiver<JoypadEvent>,
}

impl Joypad {
    pub fn new(event_recv: mpsc::Receiver<JoypadEvent>) -> Self {
        Self {
            state: Cell::new(0),
            scan_pos: Cell::new(0),
            event_recv,
        }
    }

    pub fn new_channel() -> (Joypad, JoypadEventSender) {
        let (tx, rx) = mpsc::channel();
        (Self::new(rx), tx)
    }

    pub fn new_channel_all() -> (Joypads, [JoypadEventSender; JOYPAD_COUNT]) {
        let mut joypads = vec![];
        let mut senders = vec![];
        for _ in 0..JOYPAD_COUNT {
            let (j, s) = Self::new_channel();
            joypads.push(j);
            senders.push(s);
        }
        (joypads.try_into().unwrap(), senders.try_into().unwrap())
    }

    fn poll_events(&self) {
        if let Ok(e) = self.event_recv.try_recv() {
            match &e {
                JoypadEvent::Up(button) => self
                    .state
                    .set(self.state.get() & !(1 << button.to_u8().unwrap())),
                JoypadEvent::Down(button) => self
                    .state
                    .set(self.state.get() | (1 << button.to_u8().unwrap())),
            }
        }
    }

    pub fn strobe(&self) {
        self.poll_events();
        self.scan_pos.set(0);
    }

    pub fn read(&self) -> u8 {
        let pos = self.scan_pos.get() & 0x0F;
        if pos == 0 {
            self.poll_events();
        }
        let v = ((self.state.get() >> pos) & 1) as u8;
        self.scan_pos.set(pos + 1);
        v
    }

    pub fn read_auto_low(&self) -> u8 {
        self.state.get().reverse_bits() as u8
    }

    pub fn read_auto_high(&self) -> u8 {
        (self.state.get().reverse_bits() >> 8) as u8
    }
}
