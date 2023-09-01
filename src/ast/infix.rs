/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Infix operators: times, plus, with, & par.

/// Whatever character we end up choosing instead of an upside-down ampersand.
pub const PAR: char = '@';

/// Infix operators: times, plus, with, & par.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Infix {
    /// Multiplicative conjunction, read as "times" or "tensor."
    Times,
    /// Additive disjunction, read as "plus."
    Plus,
    /// Additive conjunction, read as "with."
    With,
    /// Multiplicative disjunction, read as "par."
    Par,
}

/// Left- or right-associativity.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Associativity {
    /// Left-associative, e.g. `A * B * C`, which translates to `(A * B) * C`.
    Left,
    /// Right-associative, e.g. `A -> B -> C`, which translates to `A -> (B -> C)`.
    Right,
}

impl Infix {
    /// Whether an operator is left- or right-associative.
    #[inline]
    #[must_use]
    pub const fn associativity(self) -> Associativity {
        match self {
            Infix::Times | Infix::Plus | Infix::With | Infix::Par => Associativity::Left,
        }
    }
}

impl core::fmt::Display for Infix {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Infix::Times => '*',
                &Infix::Plus => '+',
                &Infix::With => '&',
                &Infix::Par => PAR,
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Infix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[Infix::Times, Infix::Plus, Infix::With, Infix::Par])
            .unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Infix::Times => Box::new(core::iter::empty()),
            &Infix::Plus => Box::new(core::iter::once(Infix::Times)),
            &Infix::With => Box::new([Infix::Times, Infix::Plus].into_iter()),
            &Infix::Par => Box::new([Infix::Times, Infix::Plus, Infix::With].into_iter()),
        }
    }
}
