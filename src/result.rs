use std::{error, result};

pub type Result<T = (), E = Box<dyn error::Error>> = result::Result<T, E>;

pub fn ok<T: Default, E>() -> Result<T, E> {
    return Ok(T::default());
}
