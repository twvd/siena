use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::time::{Duration, Instant};

use num_derive::ToPrimitive;
use num_traits::ToPrimitive;
use strum::{EnumCount, EnumIter, IntoEnumIterator};

pub const JOYPAD_COUNT: usize = 4;

pub type Joypads = [Joypad; JOYPAD_COUNT];
pub type JoypadEventSender = crossbeam_channel::Sender<JoypadEvent>;

#[derive(Debug, ToPrimitive, EnumCount, EnumIter, Ord, PartialOrd, Eq, PartialEq, Clone, Copy)]
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
    /// Button down on joypad
    Down(Button),
    /// Button up on joypad
    Up(Button),
    /// Joypad plugged in
    Connect,
    /// Joypad unplugged
    Disconnect,
}

#[derive(Debug)]
pub struct Joypad {
    pub state: Cell<u16>,
    pub scan_pos: Cell<u8>,
    event_recv: crossbeam_channel::Receiver<JoypadEvent>,
    sticky_time: RefCell<BTreeMap<Button, Instant>>,
    pub sticky_enabled: bool,
    pub sticky_state: Cell<u16>,
    connected: Cell<bool>,
}

impl Joypad {
    /// Time an input remains asserted after a key press in sticky mode
    const STICKY_KEYDOWN_TIME: u128 = 200;

    pub fn new(event_recv: crossbeam_channel::Receiver<JoypadEvent>) -> Self {
        Self {
            state: Cell::new(0),
            scan_pos: Cell::new(0),
            event_recv,

            sticky_time: RefCell::new(BTreeMap::from_iter(Button::iter().map(|e| {
                (
                    e,
                    Instant::now()
                        .checked_sub(Duration::from_millis(Self::STICKY_KEYDOWN_TIME as u64))
                        .unwrap(),
                )
            }))),
            sticky_enabled: false,
            sticky_state: Cell::new(0),
            connected: Cell::new(false),
        }
    }

    pub fn new_channel() -> (Joypad, JoypadEventSender) {
        let (tx, rx) = crossbeam_channel::unbounded();
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
        // Clear sticky buttons
        for b in Button::iter() {
            if self
                .sticky_time
                .borrow()
                .get(&b)
                .unwrap()
                .elapsed()
                .as_millis()
                > Self::STICKY_KEYDOWN_TIME
            {
                self.sticky_state
                    .set(self.sticky_state.get() & !(1 << b.to_u16().unwrap()));
            }
        }

        if self.event_recv.is_empty() {
            return;
        }

        while let Ok(e) = self.event_recv.try_recv() {
            match &e {
                JoypadEvent::Up(button) => self
                    .state
                    .set(self.state.get() & !(1 << button.to_u16().unwrap())),
                JoypadEvent::Down(button) => {
                    self.sticky_time
                        .borrow_mut()
                        .insert(*button, Instant::now());
                    self.state
                        .set(self.state.get() | (1 << button.to_u16().unwrap()));
                    self.sticky_state
                        .set(self.sticky_state.get() | (1 << button.to_u16().unwrap()));
                }
                JoypadEvent::Connect => self.connected.set(true),
                JoypadEvent::Disconnect => self.connected.set(false),
            }
        }
        if !self.connected.get() {
            self.sticky_state.set(0);
            self.state.set(0);
        }
    }

    pub fn strobe(&self) {
        self.poll_events();
        self.scan_pos.set(0);
    }

    pub fn advance(&self, steps: u8) {
        self.scan_pos.set(self.scan_pos.get().saturating_add(steps));
    }

    pub fn read(&self) -> u8 {
        let pos = self.scan_pos.get();
        if pos == 0 {
            self.poll_events();
        }
        self.advance(1);

        if pos >= 16 {
            if self.connected.get() {
                1
            } else {
                0
            }
        } else {
            ((self.get_state() >> pos) & 1) as u8
        }
    }

    fn get_state(&self) -> u16 {
        if self.sticky_enabled {
            self.sticky_state.get()
        } else {
            self.state.get()
        }
    }

    pub fn read_auto_low(&self) -> u8 {
        self.poll_events();
        self.get_state().reverse_bits() as u8
    }

    pub fn read_auto_high(&self) -> u8 {
        self.poll_events();
        (self.get_state().reverse_bits() >> 8) as u8
    }
}
