//! This module contains various tools that help the ergonomics of this crate.

mod fallible_iterator;
pub(crate) mod logging;
pub(crate) mod serde_bridge;

pub(crate) use fallible_iterator::*;
