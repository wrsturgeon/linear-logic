/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Infix operators: times, plus, with, & par.

/// Whatever character we end up choosing instead of an upside-down ampersand.
pub const PAR: char = '@';
/// Par as a string.
pub const PAR_STR: &str = "@";

/// Infix operators: times, plus, with, & par.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Infix {
    /// Lollipop operator: basically resource-aware implication.
    Lollipop,
    /// Additive disjunction, read as "plus."
    Plus,
    /// Additive conjunction, read as "with."
    With,
    /// Multiplicative disjunction, read as "par."
    Par,
    /// Multiplicative conjunction, read as "times" or "tensor."
    Times,
}

/// Left- or right-associativity.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Associativity {
    /// Left-associative: e.g. `A * B * C`, which translates to `(A * B) * C`.
    Left,
    /// Right-associative: e.g. `A -> B -> C`, which translates to `A -> (B -> C)`.
    Right,
}

impl Infix {
    /// Whether an operator is left- or right-associative.
    #[inline]
    #[must_use]
    pub const fn associativity(self) -> Associativity {
        match self {
            Infix::Times | Infix::Plus | Infix::With | Infix::Par => Associativity::Left,
            Infix::Lollipop => Associativity::Right,
        }
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_left_of(self, other: Infix) -> bool {
        match self.cmp(&other) {
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => self.associativity() == Associativity::Right,
            core::cmp::Ordering::Greater => false,
        }
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_right_of(self, other: Infix) -> bool {
        match self.cmp(&other) {
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => self.associativity() == Associativity::Left,
            core::cmp::Ordering::Greater => false,
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
                &Infix::Times => "*",
                &Infix::Plus => "+",
                &Infix::With => "&",
                &Infix::Par => PAR_STR,
                &Infix::Lollipop => "->",
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Infix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[
            Infix::Times,
            Infix::Plus,
            Infix::With,
            Infix::Par,
            Infix::Lollipop,
        ])
        .unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Infix::Plus => Box::new(core::iter::empty()),
            &Infix::With => Box::new(core::iter::once(Infix::Plus)),
            &Infix::Par => Box::new([Infix::Plus, Infix::With].into_iter()),
            &Infix::Times => Box::new([Infix::Plus, Infix::With, Infix::Par].into_iter()),
            &Infix::Lollipop => {
                Box::new([Infix::Plus, Infix::With, Infix::Par, Infix::Times].into_iter())
            }
        }
    }
}
