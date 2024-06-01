use enum_map::{Enum, EnumMap};
use strum::EnumIter;

const JOYPAD_UNUSED: u8 = (1 << 7) | (1 << 6);
const JOYPAD_SELECT_MASK: u8 = 0x30;
const JOYPAD_SELECT_ACTION: u8 = 1 << 5;
const JOYPAD_SELECT_DIRECTION: u8 = 1 << 4;
const JOYPAD_IN_DOWN_START: u8 = 1 << 3;
const JOYPAD_IN_UP_SELECT: u8 = 1 << 2;
const JOYPAD_IN_LEFT_B: u8 = 1 << 1;
const JOYPAD_IN_RIGHT_A: u8 = 1 << 0;

#[derive(Debug, Copy, Clone, Eq, PartialEq, EnumIter, Ord, PartialOrd, Enum)]
pub enum Button {
    DPadUp,
    DPadDown,
    DPadLeft,
    DPadRight,
    A,
    B,
    Start,
    Select,
}

pub struct Joypad {
    /// Joypad select bits
    select: u8,

    /// Active inputs
    state: EnumMap<Button, bool>,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            select: 0,
            state: EnumMap::default(),
        }
    }

    pub fn set(&mut self, b: Button, v: bool) {
        self.state[b] = v;
    }

    fn read_bit(&self, b: Button, bit: u8) -> u8 {
        if self.state[b] {
            0
        } else {
            bit
        }
    }

    pub fn read(&self) -> u8 {
        JOYPAD_UNUSED
            | self.select
            | match !self.select & JOYPAD_SELECT_MASK {
                JOYPAD_SELECT_ACTION => {
                    self.read_bit(Button::Start, JOYPAD_IN_DOWN_START)
                        | self.read_bit(Button::Select, JOYPAD_IN_UP_SELECT)
                        | self.read_bit(Button::A, JOYPAD_IN_RIGHT_A)
                        | self.read_bit(Button::B, JOYPAD_IN_LEFT_B)
                }
                JOYPAD_SELECT_DIRECTION => {
                    self.read_bit(Button::DPadDown, JOYPAD_IN_DOWN_START)
                        | self.read_bit(Button::DPadUp, JOYPAD_IN_UP_SELECT)
                        | self.read_bit(Button::DPadRight, JOYPAD_IN_RIGHT_A)
                        | self.read_bit(Button::DPadLeft, JOYPAD_IN_LEFT_B)
                }
                _ => 0x00,
            }
    }

    pub fn write(&mut self, val: u8) {
        self.select = val & JOYPAD_SELECT_MASK;
    }
}
