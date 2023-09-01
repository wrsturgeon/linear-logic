/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Prefix operators: bang & quest.

/// Prefix operators: bang & quest.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Prefix {
    /// The "of course" operator, read as "bang."
    Bang,
    /// The "why not" operator, read as "quest."
    Quest,
}

impl core::fmt::Display for Prefix {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Prefix::Bang => '!',
                &Prefix::Quest => '?',
            }
        )
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Prefix {
    #[inline]
    #[allow(clippy::unwrap_used)]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[Prefix::Bang, Prefix::Quest]).unwrap()
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Prefix::Bang => Box::new(core::iter::empty()),
            &Prefix::Quest => Box::new(core::iter::once(Prefix::Bang)),
        }
    }
}
