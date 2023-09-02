/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Result, error, _**or** warning_.

/// Result, error, _**or** warning_.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Triage<T, W, E> {
    /// Successful result.
    Okay(T),
    /// Result with a warning.
    Warn(T, W),
    /// Error and no result.
    Error(E),
}

/// Either a warning or an error.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Severity<W, E> {
    /// Not-necessarily-fatal warning.
    Warn(W),
    /// Fatal error.
    Error(E),
}

#[allow(clippy::missing_const_for_fn)] // destructor issue
impl<T, W, E> Triage<T, W, E> {
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
    pub fn permissive(self) -> Result<T, E> {
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
                Triage::Okay(u) | Triage::Warn(u, _) => Triage::Warn(u, w),
                Triage::Error(e) => Triage::Error(e),
            },
            Triage::Error(e) => Triage::Error(e),
        }
    }
}

/// Map `Some` to `Triage::Okay` and `None` to `Triage::Error`.
#[allow(clippy::module_name_repetitions)]
pub trait OptionTriage {
    /// Output of a successful call.
    type Value;
    /// If ended, `Triage::Error(parse::Error::End)`; otherwise, wrap in `Triage::Okay`.
    fn triage<W, E>(self, err: E) -> Triage<Self::Value, W, E>;
}

impl<T> OptionTriage for Option<T> {
    type Value = T;
    #[inline(always)]
    fn triage<W, E>(self, err: E) -> Triage<Self::Value, W, E> {
        self.map_or(Triage::Error(err), Triage::Okay)
    }
}

/// Map `Some` to `Triage::Warn` and `None` to the provided value.
pub trait OptionWarn {
    /// Type of warning.
    type Warning;
    /// Map `Some` to `Triage::Warn` and `None` to `Triage::Okay`.
    fn warn_or<T, E>(self, value: T) -> Triage<T, Self::Warning, E>;
    /// Map `Some` to `Triage::Warn` and `None` to the provided value.
    fn warn_and_then<T, E>(self, value: Triage<T, Self::Warning, E>)
        -> Triage<T, Self::Warning, E>;
}

impl<W> OptionWarn for Option<W> {
    type Warning = W;
    #[inline(always)]
    fn warn_or<T, E>(self, value: T) -> Triage<T, Self::Warning, E> {
        match self {
            None => Triage::Okay(value),
            Some(w) => Triage::Warn(value, w),
        }
    }
    #[inline(always)]
    fn warn_and_then<T, E>(
        self,
        value: Triage<T, Self::Warning, E>,
    ) -> Triage<T, Self::Warning, E> {
        match self {
            None => value,
            Some(w) => match value {
                Triage::Okay(v) | Triage::Warn(v, _) => Triage::Warn(v, w),
                Triage::Error(e) => Triage::Error(e),
            },
        }
    }
}
