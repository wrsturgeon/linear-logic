/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Name of a variable.

use crate::{
    ast::{Funky, Simplified},
    parse::{Error, Warning},
    Spanned, Triage,
};

/// Base case: a bound variable name, 0, 1, bottom, or top.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Atomic {
    /// A bound variable name.
    Bound(String),
    /// `0`, the unit for `+`.
    Zero,
    /// `1`, the unit for `*`.
    One,
    /// Bottom (usually represented with an up-tack), the unit for par.
    Bottom,
    /// Top, the unit for `&`.
    Top,
}

impl Atomic {
    /// Screen for invalid names.
    /// # Errors
    /// When a would-be name would be invalid.
    #[inline]
    #[allow(dead_code)]
    #[cfg(any(test, feature = "quickcheck"))]
    pub(crate) fn from_str(s: &str) -> Triage<Self, Warning, Error> {
        let mut acc = Self::Bound(String::new());
        for c in s.chars() {
            match acc.push::<core::convert::Infallible>(c, usize::MAX) {
                Triage::Okay(()) => {}
                Triage::Warn((), _literally_impossible) => {
                    #[cfg(test)]
                    #[allow(clippy::unreachable)]
                    {
                        unreachable!()
                    }
                    #[cfg(not(test))]
                    #[allow(unsafe_code)]
                    // SAFETY:
                    // Type literally can't be constructed.
                    unsafe {
                        core::hint::unreachable_unchecked()
                    }
                }
                Triage::Error(e) => return Triage::Error(e),
            }
        }
        Triage::Okay(acc)
    }

    /// Add a character if it's valid.
    #[inline]
    pub(crate) fn push<W: Ord + core::fmt::Display>(
        &mut self,
        c: char,
        index: usize,
    ) -> Triage<(), W, Error> {
        #[allow(clippy::panic)]
        let &mut Self::Bound(ref mut s) = self else {
            panic!("Trying to push a character to a non-string atomic");
        };
        if c.is_alphabetic() && !c.is_uppercase() {
            s.push(c);
            Triage::Okay(())
        } else {
            Triage::Error(Spanned {
                msg: Error::InvalidName,
                index,
            })
        }
    }

    /// Screen for invalid names.
    /// # Errors
    /// When a would-be name would be invalid.
    #[inline]
    pub(crate) fn from_char(c: char, index: usize) -> Triage<Self, Warning, Error> {
        let mut acc = Self::Bound(String::new());
        acc.push(c, index).map(|()| acc)
    }

    /// Dual, i.e. linear negation.
    #[must_use]
    #[inline(always)]
    #[allow(clippy::missing_const_for_fn)] // :_(
    pub fn simplified_dual(self) -> Simplified {
        match self {
            Self::Bound(s) => Simplified::Dual(s),
            Self::Zero => Simplified::Atomic(Self::Top),
            Self::Top => Simplified::Atomic(Self::Zero),
            Self::One => Simplified::Atomic(Self::Bottom),
            Self::Bottom => Simplified::Atomic(Self::One),
        }
    }

    /// Dual, i.e. linear negation.
    #[must_use]
    #[inline(always)]
    #[allow(clippy::missing_const_for_fn)] // :_(
    pub fn funky_dual(self) -> Funky {
        match self {
            Self::Bound(s) => Funky::Dual(s),
            Self::Zero => Funky::Atomic(Self::Top),
            Self::Top => Funky::Atomic(Self::Zero),
            Self::One => Funky::Atomic(Self::Bottom),
            Self::Bottom => Funky::Atomic(Self::One),
        }
    }

    /// If this is either not a string or an empty string.
    #[must_use]
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        match self {
            &Self::Bound(ref s) => s.is_empty(),
            &(Self::Zero | Self::Top | Self::One | Self::Bottom) => false,
        }
    }

    /// Check if we might still be reading a name.
    /// If we are, return a mutable reference to it.
    #[inline]
    pub(crate) fn reading_name(&mut self) -> Option<&mut Atomic> {
        match self {
            &mut Self::Bound(_) => Some(self),
            &mut (Self::Zero | Self::One | Self::Bottom | Self::Top) => None,
        }
    }

    /// Mutate leaves (names) with some function.
    #[inline]
    #[must_use]
    pub fn map<F: Fn(String) -> String>(self, f: F) -> Self {
        match self {
            Self::Bound(s) => Self::Bound(f(s)),
            Self::Zero | Self::One | Self::Bottom | Self::Top => self,
        }
    }

    /// Mutable references to all names.
    #[inline]
    pub fn names(&mut self) -> Vec<&mut String> {
        match self {
            &mut Self::Bound(ref mut s) => vec![s],
            &mut (Self::Zero | Self::One | Self::Bottom | Self::Top) => vec![],
        }
    }
}

impl core::fmt::Display for Atomic {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            &Self::Bound(ref s) => write!(f, "{s}"),
            &Self::Zero => write!(f, "0"),
            &Self::One => write!(f, "1"),
            &Self::Bottom => write!(f, "_"),
            &Self::Top => write!(f, "T"),
        }
    }
}

impl From<Atomic> for String {
    #[inline(always)]
    fn from(value: Atomic) -> Self {
        match value {
            Atomic::Bound(s) => s,
            Atomic::Zero => "0".to_owned(),
            Atomic::One => "1".to_owned(),
            Atomic::Bottom => "_".to_owned(),
            Atomic::Top => "T".to_owned(),
        }
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Atomic {
    #[inline]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        if bool::arbitrary(g) {
            #[allow(clippy::unwrap_used)]
            g.choose(&[Self::Zero, Self::One, Self::Bottom, Self::Top])
                .unwrap()
                .clone()
        } else {
            loop {
                let Triage::Okay(mut acc) = Self::from_char(char::arbitrary(g), usize::MAX) else {
                    continue;
                };
                let s = String::arbitrary(g);
                for c in s.chars() {
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = acc.push::<core::convert::Infallible>(c, usize::MAX);
                }
                return acc;
            }
        }
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Self::Zero => Box::new(core::iter::empty()),
            &Self::One => Box::new(core::iter::once(Self::Zero)),
            &Self::Bottom => Box::new([Self::Zero, Self::One].into_iter()),
            &Self::Top => Box::new([Self::Zero, Self::One, Self::Bottom].into_iter()),
            &Self::Bound(ref s) => {
                Box::new([Self::Zero, Self::One, Self::Bottom].into_iter().chain(
                    s.shrink().filter_map(|st| {
                        let mut acc = Self::Bound(String::new());
                        for c in st.chars() {
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = acc.push::<core::convert::Infallible>(c, usize::MAX);
                        }
                        (!acc.is_empty()).then_some(acc)
                    }),
                ))
            }
        }
    }
}
