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

    /// Convert to a (host)-native color (RGB888)
    pub fn to_native(&self) -> Color {
        // TODO better put this in the frontend? I'm not sure..
        (
            (((self.0 >> 0) & 0x1F) as u8) << 3,  // Red, 5-bit
            (((self.0 >> 5) & 0x1F) as u8) << 3,  // Green, 5-bit
            (((self.0 >> 10) & 0x1F) as u8) << 3, // Blue, 5-bit
        )
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
}

impl std::ops::Add for SnesColor {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(
            ((self.0 & 0x1F) + (other.0 & 0x1F)) & 0x1F
                | ((self.0 & 0x3E0) + (other.0 & 0x3E0)) & 0x3E0
                | ((self.0 & 0x7C00) + (other.0 & 0x7C00)) & 0x7C00,
        )
    }
}

impl std::ops::Sub for SnesColor {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self(
            ((self.0 & 0x1F) - (other.0 & 0x1F)) & 0x1F
                | ((self.0 & 0x3E0) - (other.0 & 0x3E0)) & 0x3E0
                | ((self.0 & 0x7C00) - (other.0 & 0x7C00)) & 0x7C00,
        )
    }
}

impl std::ops::AddAssign for SnesColor {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl std::ops::SubAssign for SnesColor {
    fn sub_assign(&mut self, other: Self) {
        *self = *self - other;
    }
}
