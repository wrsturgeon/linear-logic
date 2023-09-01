/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Name of a variable.

use crate::{
    parse::{Error, Warning},
    Triage,
};

/// Name of a variable.
#[repr(transparent)]
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Name(char);

impl Name {
    /// Screen for invalid names.
    /// # Errors
    /// When a would-be name would be invalid.
    #[inline]
    #[must_use]
    pub fn from_char(c: char) -> Triage<Self, Warning, Error> {
        if c.is_alphabetic() {
            Triage::Okay(Self(c))
        } else {
            Triage::Error(Error::InvalidName(c))
        }
    }
}

impl core::fmt::Display for Name {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&self.0, f)
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Name {
    #[inline]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        loop {
            let c = char::arbitrary(g);
            if c.is_alphabetic() {
                return Self(c);
            }
        }
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.0.shrink().filter(|c| c.is_alphabetic()).map(Self))
    }
}
