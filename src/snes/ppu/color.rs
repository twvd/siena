use num_traits::int::PrimInt;
use num_traits::AsPrimitive;

use crate::frontend::Color;

/// RGB555 SNES-native color
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct SnesColor(u16);

impl SnesColor {
    pub const BLACK: Self = Self(0);

    pub fn from(c: u16) -> SnesColor {
        Self(c)
    }

    pub fn from_rgb5(r: u8, g: u8, b: u8) -> SnesColor {
        Self((r as u16 & 0x1F) | (g as u16 & 0x1F) << 5 | (b as u16 & 0x1F) << 10)
    }

    /// Convert to a (host)-native color (RGB888)
    pub fn to_native(&self) -> Color {
        // TODO better put this in the frontend? I'm not sure..
        (
            self.r() << 3, // Red, 5-bit
            self.g() << 3, // Green, 5-bit
            self.b() << 3, // Blue, 5-bit
        )
    }

    pub fn r(&self) -> u8 {
        (self.0 & 0x1F) as u8
    }

    pub fn g(&self) -> u8 {
        ((self.0 >> 5) & 0x1F) as u8
    }

    pub fn b(&self) -> u8 {
        ((self.0 >> 10) & 0x1F) as u8
    }

    /// Replaces the red intensity
    pub fn with_r(&self, val: u8) -> Self {
        debug_assert!(val <= 0x1F);
        Self((u16::from(val) & 0x1F) | (self.0 & !0x1F))
    }

    /// Replaces the green intensity
    pub fn with_g(&self, val: u8) -> Self {
        debug_assert!(val <= 0x1F);
        Self(((u16::from(val) & 0x1F) << 5) | (self.0 & !(0x1F << 5)))
    }

    /// Replaces the blue intensity
    pub fn with_b(&self, val: u8) -> Self {
        debug_assert!(val <= 0x1F);
        Self(((u16::from(val) & 0x1F) << 10) | (self.0 & !(0x1F << 10)))
    }

    /// Apply brightness (0 - 15)
    pub fn apply_brightness<T: PrimInt + AsPrimitive<u32>>(&self, brightness: T) -> Self {
        if brightness == T::zero() {
            return Self::BLACK;
        }

        let brightness: u32 = (brightness + T::one()).as_();
        debug_assert!(brightness <= 16);
        let t = self.0 as u32;

        Self(
            (((((t & 0x7C1F) * brightness) & 0x7C1F0) | (((t & 0x3E0) * brightness) & 0x3E00)) >> 4)
                as u16,
        )
    }

    /// Color math add
    pub fn cm_add(&self, other: &SnesColor, div2: bool) -> SnesColor {
        let div = if div2 { 2 } else { 1 };
        Self::from_rgb5(
            (self.r() + other.r()) / div,
            (self.g() + other.g()) / div,
            (self.b() + other.b()) / div,
        )
    }

    /// Color math subtract
    pub fn cm_sub(&self, other: &SnesColor, div2: bool) -> SnesColor {
        let div = if div2 { 2 } else { 1 };
        Self::from_rgb5(
            (self.r().saturating_sub(other.r())) / div,
            (self.g().saturating_sub(other.g())) / div,
            (self.b().saturating_sub(other.b())) / div,
        )
    }
}
