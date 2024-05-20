use anyhow::Result;

pub const ONE_MCYCLE: usize = 4;

/// Representation of X amount of ticks (T-cycles)
#[derive(Clone, Copy)]
pub enum Ticks {
    /// X T-cycles, while in normal speed mode
    NormalSpeed(usize),
    /// X T-cycles, while in double speed mode
    DoubleSpeed(usize),
}

impl Ticks {
    /// Create new (from T-cycles), normal speed
    pub fn from_t(t: usize) -> Self {
        Self::from_t_xs(t, false)
    }

    /// Create new (from T-cycles), double speed
    pub fn from_t_ds(t: usize) -> Self {
        Self::from_t_xs(t, true)
    }

    /// Create new (from T-cycles), normal/double speed
    pub fn from_t_xs(t: usize, ds: bool) -> Self {
        if ds {
            Self::DoubleSpeed(t)
        } else {
            Self::NormalSpeed(t)
        }
    }

    /// Create new (from M-cycles)
    pub fn from_m_xs(m: usize, ds: bool) -> Self {
        Self::from_t_xs(m * ONE_MCYCLE, ds)
    }

    /// Gets the amount of T-cycles, supporting double speed.
    pub fn get_t_ds(&self) -> usize {
        match self {
            Self::NormalSpeed(t) => *t,
            Self::DoubleSpeed(t) => *t,
        }
    }

    /// Gets the amount of M-cycles, supporting double speed.
    pub fn get_m_ds(&self) -> usize {
        self.get_t_ds() / ONE_MCYCLE
    }

    /// Gets the amount of T-cycles, NOT supporting double speed.
    pub fn get_t_no_ds(&self) -> usize {
        match self {
            Self::NormalSpeed(t) => *t,
            Self::DoubleSpeed(t) => *t / 2,
        }
    }

    /// Gets the amount of M-cycles, NOT supporting double speed.
    pub fn get_m_no_ds(&self) -> usize {
        self.get_t_no_ds() / ONE_MCYCLE
    }

    /// Test if this is in double speed mode
    pub fn is_double_speed(&self) -> bool {
        matches!(self, Self::DoubleSpeed(_))
    }
}

pub trait Tickable {
    fn tick(&mut self, ticks: Ticks) -> Result<()>;
}
