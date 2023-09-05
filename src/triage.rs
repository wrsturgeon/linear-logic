/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Result, error, _**or** warning_.

use core::fmt::Display;

/// Result, error, _**or** warning_.
#[must_use]
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Triage<T, W: Ord + Display, E: Display> {
    /// Successful result.
    Okay(T),
    /// Result with a warning.
    Warn(T, Spanned<W>),
    /// Error and no result.
    Error(Spanned<E>),
}

/// Error with blame on a span of input.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Spanned<M: Display> {
    /// Error/warning message.
    pub msg: M,
    /// Index of the first character that caused this error.
    pub index: usize,
}

impl<M: PartialOrd + Display> PartialOrd for Spanned<M> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match self.msg.partial_cmp(&other.msg) {
            None | Some(core::cmp::Ordering::Equal) => Some(self.index.cmp(&other.index).reverse()),
            diff @ Some(core::cmp::Ordering::Less | core::cmp::Ordering::Greater) => diff,
        }
    }
}

impl<M: Ord + Display> Ord for Spanned<M> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        match self.msg.cmp(&other.msg) {
            core::cmp::Ordering::Equal => self.index.cmp(&other.index).reverse(),
            diff @ (core::cmp::Ordering::Less | core::cmp::Ordering::Greater) => diff,
        }
    }
}

impl<M: Display> Display for Spanned<M> {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Incident ({}) at character #{}.", self.msg, self.index)
    }
}

/// Either a warning or an error.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Severity<W: Ord + Display, E: Display> {
    /// Not-necessarily-fatal warning.
    Warn(Spanned<W>),
    /// Fatal error.
    Error(Spanned<E>),
}

#[allow(clippy::missing_const_for_fn)] // destructor issue
impl<T, W: Ord + Display, E: Display> Triage<T, W, E> {
    /// Turn warningss into errors and return a `Result`.
    /// # Errors
    /// Both `Error` and `Warning`.
    #[inline]
    pub fn strict(self) -> Result<T, Severity<W, E>> {
        match self {
            Triage::Okay(v) => Ok(v),
            Triage::Warn(_, w) => Err(Severity::Warn(w)),
            Triage::Error(e) => Err(Severity::Error(e)),
        }
    }

    /// Ignore warnings and return a `Result`.
    /// # Errors
    /// Only `Error`, not `Warning`.
    #[inline]
    pub fn permissive(self) -> Result<T, Spanned<E>> {
        match self {
            Triage::Okay(v) | Triage::Warn(v, _) => Ok(v),
            Triage::Error(e) => Err(e),
        }
    }

    /// Apply a function to a successful or warned value.
    #[inline]
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Triage<U, W, E> {
        match self {
            Triage::Okay(v) => Triage::Okay(f(v)),
            Triage::Warn(v, w) => Triage::Warn(f(v), w),
            Triage::Error(e) => Triage::Error(e),
        }
    }

    /// Apply a function to a successful or warned value.
    /// monadic af <3
    #[inline]
    pub fn and_then<U, F: FnOnce(T) -> Triage<U, W, E>>(self, f: F) -> Triage<U, W, E> {
        match self {
            Triage::Okay(v) => f(v),
            Triage::Warn(v, w) => match f(v) {
                Triage::Okay(u) => Triage::Warn(u, w),
                Triage::Warn(u, ww) => Triage::Warn(u, w.max(ww)),
                Triage::Error(e) => Triage::Error(e),
            },
            Triage::Error(e) => Triage::Error(e),
        }
    }
}

impl<T: Display, W: Ord + Display, E: Display> Display for Triage<T, W, E> {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            &Self::Okay(ref v) => write!(f, "{v}"),
            &Self::Warn(ref v, ref w) => write!(f, "WARNING: {w}. Try this: {v}"),
            &Self::Error(ref e) => write!(f, "{e}"),
        }
    }
}
