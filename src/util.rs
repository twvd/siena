use num::traits::{WrappingShl, WrappingShr};
use num::Integer;
use std::mem::size_of_val;

/// Sign-extend a value
pub fn sign_extend<T: Integer + WrappingShl + WrappingShr>(val: T, nbits: u32) -> T {
    let notherbits = size_of_val(&val) as u32 * 8 - nbits;
    val.wrapping_shl(notherbits).wrapping_shr(notherbits)
}
