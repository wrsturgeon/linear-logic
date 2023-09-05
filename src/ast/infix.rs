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

/// Left- or right-associativity.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Associativity {
    /// Left-associative: e.g. `A * B * C`, which translates to `(A * B) * C`.
    Left,
    /// Right-associative: e.g. `A -> B -> C`, which translates to `A -> (B -> C)`.
    Right,
}

/// Times, plus, with, par, & lollipop.
#[allow(clippy::exhaustive_enums, clippy::module_name_repetitions)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum UnsimplifiedInfix {
    /// Lollipop: basically resource-aware implication.
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

impl UnsimplifiedInfix {
    /// Whether an operator is left- or right-associative.
    #[inline]
    #[must_use]
    pub const fn associativity(self) -> Associativity {
        match self {
            Self::Times | Self::Plus | Self::With | Self::Par => Associativity::Left,
            Self::Lollipop => Associativity::Right,
        }
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_left_of(self, other: Self) -> bool {
        match self.cmp(&other) {
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => self.associativity() == Associativity::Right,
            core::cmp::Ordering::Greater => false,
        }
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_right_of(self, other: Self) -> bool {
        match self.cmp(&other) {
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => self.associativity() == Associativity::Left,
            core::cmp::Ordering::Greater => false,
        }
    }
}

impl core::fmt::Display for UnsimplifiedInfix {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Self::Times => "*",
                &Self::Plus => "+",
                &Self::With => "&",
                &Self::Par => PAR_STR,
                &Self::Lollipop => "->",
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for UnsimplifiedInfix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[
            Self::Times,
            Self::Plus,
            Self::With,
            Self::Par,
            Self::Lollipop,
        ])
        .unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Self::Plus => Box::new(core::iter::empty()),
            &Self::With => Box::new(core::iter::once(Self::Plus)),
            &Self::Par => Box::new([Self::Plus, Self::With].into_iter()),
            &Self::Times => Box::new([Self::Plus, Self::With, Self::Par].into_iter()),
            &Self::Lollipop => {
                Box::new([Self::Plus, Self::With, Self::Par, Self::Times].into_iter())
            }
        }
    }
}

/// Times, plus, with, & par (no lollipop).
#[allow(clippy::exhaustive_enums, clippy::module_name_repetitions)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SimplifiedInfix {
    /// Additive disjunction, read as "plus."
    Plus,
    /// Additive conjunction, read as "with."
    With,
    /// Multiplicative disjunction, read as "par."
    Par,
    /// Multiplicative conjunction, read as "times" or "tensor."
    Times,
}

impl From<SimplifiedInfix> for UnsimplifiedInfix {
    #[inline(always)]
    fn from(value: SimplifiedInfix) -> Self {
        match value {
            SimplifiedInfix::Plus => UnsimplifiedInfix::Plus,
            SimplifiedInfix::With => UnsimplifiedInfix::With,
            SimplifiedInfix::Par => UnsimplifiedInfix::Par,
            SimplifiedInfix::Times => UnsimplifiedInfix::Times,
        }
    }
}

impl SimplifiedInfix {
    /// Whether an operator is left- or right-associative.
    #[inline]
    #[must_use]
    pub const fn associativity(self) -> Associativity {
        Associativity::Left
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_left_of(self, other: Self) -> bool {
        self < other
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_right_of(self, other: Self) -> bool {
        self <= other
    }
}

impl core::fmt::Display for SimplifiedInfix {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Self::Times => '*',
                &Self::Plus => '+',
                &Self::With => '&',
                &Self::Par => PAR,
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for SimplifiedInfix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[Self::Times, Self::Plus, Self::With, Self::Par])
            .unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Self::Plus => Box::new(core::iter::empty()),
            &Self::With => Box::new(core::iter::once(Self::Plus)),
            &Self::Par => Box::new([Self::Plus, Self::With].into_iter()),
            &Self::Times => Box::new([Self::Plus, Self::With, Self::Par].into_iter()),
        }
    }
}

/// Times, plus, with, & par (no lollipop).
#[allow(clippy::exhaustive_enums, clippy::module_name_repetitions)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FunkyInfix {
    /// Lollipop: basically resource-aware implication.
    Lollipop,
    /// Additive disjunction, read as "plus."
    Plus,
    /// Additive conjunction, read as "with."
    With,
    /// Multiplicative conjunction, read as "times" or "tensor."
    Times,
}

impl From<FunkyInfix> for UnsimplifiedInfix {
    #[inline(always)]
    fn from(value: FunkyInfix) -> Self {
        match value {
            FunkyInfix::Plus => UnsimplifiedInfix::Plus,
            FunkyInfix::With => UnsimplifiedInfix::With,
            FunkyInfix::Lollipop => UnsimplifiedInfix::Lollipop,
            FunkyInfix::Times => UnsimplifiedInfix::Times,
        }
    }
}

impl FunkyInfix {
    /// Whether an operator is left- or right-associative.
    #[inline]
    #[must_use]
    pub const fn associativity(self) -> Associativity {
        match self {
            Self::Lollipop => Associativity::Right,
            Self::Plus | Self::With | Self::Times => Associativity::Left,
        }
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_left_of(self, other: Self) -> bool {
        match self.cmp(&other) {
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => self.associativity() == Associativity::Right,
            core::cmp::Ordering::Greater => false,
        }
    }

    /// Whether an operator to the right would bind more strongly.
    #[inline]
    #[must_use]
    pub fn weaker_to_the_right_of(self, other: Self) -> bool {
        match self.cmp(&other) {
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => self.associativity() == Associativity::Left,
            core::cmp::Ordering::Greater => false,
        }
    }
}

impl core::fmt::Display for FunkyInfix {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Self::Times => "*",
                &Self::Plus => "+",
                &Self::With => "&",
                &Self::Lollipop => "->",
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for FunkyInfix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[Self::Times, Self::Plus, Self::With, Self::Lollipop])
            .unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Self::Plus => Box::new(core::iter::empty()),
            &Self::With => Box::new(core::iter::once(Self::Plus)),
            &Self::Lollipop => Box::new([Self::Plus, Self::With].into_iter()),
            &Self::Times => Box::new([Self::Plus, Self::With, Self::Lollipop].into_iter()),
        }
    }
}
