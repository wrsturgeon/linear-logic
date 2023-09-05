/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Prefix operators: bang, quest, & dual.

/// Unsimplified prefix operators: bang, quest, & dual.
#[allow(clippy::exhaustive_enums, clippy::module_name_repetitions)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum UnsimplifiedPrefix {
    /// The "of course" operator, read as "bang."
    Bang,
    /// The "why not" operator, read as "quest."
    Quest,
    /// The dual operator, written in linear logic as a raised bottom.
    Dual,
}

impl core::fmt::Display for UnsimplifiedPrefix {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Self::Bang => '!',
                &Self::Quest => '?',
                &Self::Dual => '~',
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for UnsimplifiedPrefix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[Self::Bang, Self::Quest, Self::Dual]).unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Self::Bang => Box::new(core::iter::empty()),
            &Self::Quest => Box::new(core::iter::once(Self::Bang)),
            &Self::Dual => Box::new([Self::Bang, Self::Quest].into_iter()),
        }
    }
}

/// Simplified prefix operators: bang, quest, & dual.
#[allow(clippy::exhaustive_enums, clippy::module_name_repetitions)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SimplifiedPrefix {
    /// The "of course" operator, read as "bang."
    Bang,
    /// The "why not" operator, read as "quest."
    Quest,
}

impl From<SimplifiedPrefix> for UnsimplifiedPrefix {
    #[inline(always)]
    fn from(value: SimplifiedPrefix) -> Self {
        match value {
            SimplifiedPrefix::Bang => UnsimplifiedPrefix::Bang,
            SimplifiedPrefix::Quest => UnsimplifiedPrefix::Quest,
        }
    }
}

impl core::fmt::Display for SimplifiedPrefix {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Self::Bang => '!',
                &Self::Quest => '?',
            }
        )
    }
}

impl SimplifiedPrefix {
    /// Dual: i.e. just flip `Bang` & `Quest`.
    #[must_use]
    #[inline(always)]
    pub const fn dual(self) -> Self {
        match self {
            Self::Bang => Self::Quest,
            Self::Quest => Self::Bang,
        }
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for SimplifiedPrefix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[Self::Bang, Self::Quest]).unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Self::Bang => Box::new(core::iter::empty()),
            &Self::Quest => Box::new(core::iter::once(Self::Bang)),
        }
    }
}
