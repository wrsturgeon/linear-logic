/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Simplified linear-logic expressions as heap trees.

use crate::ast::{
    Funky, FunkyInfix, Name, SimplifiedInfix as Infix, SimplifiedPrefix as Prefix, Unsimplified,
    UnsimplifiedPrefix,
};

/// Simplified linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Simplified {
    /// Raw name: e.g. `A`.
    Value(Name),
    /// Dual of a raw value: e.g. `~A`.
    Dual(Name),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Simplified>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<Simplified>, Infix, Box<Simplified>),
}

impl From<Simplified> for Unsimplified {
    #[inline]
    fn from(value: Simplified) -> Self {
        match value {
            Simplified::Value(name) => Unsimplified::Value(name),
            Simplified::Dual(name) => Unsimplified::Unary(
                UnsimplifiedPrefix::Dual,
                Box::new(Unsimplified::Value(name)),
            ),
            Simplified::Unary(op, arg) => Unsimplified::Unary(op.into(), Box::new((*arg).into())),
            Simplified::Binary(lhs, op, rhs) => {
                Unsimplified::Binary(Box::new((*lhs).into()), op.into(), Box::new((*rhs).into()))
            }
        }
    }
}

impl core::fmt::Display for Simplified {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(&Unsimplified::from(self.clone()), f)
    }
}

impl Simplified {
    /// Compute the dual for this expression without non-atomic duals or lollipops.
    #[inline]
    #[must_use]
    pub fn dual(self) -> Self {
        match self {
            Self::Value(name) => Self::Dual(name),
            Self::Dual(name) => Self::Value(name),
            Self::Unary(op, arg) => Self::Unary(op.dual(), Box::new(arg.dual())),
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Plus => {
                    Self::Binary(Box::new(lhs.dual()), Infix::With, Box::new(rhs.dual()))
                }
                Infix::With => {
                    Self::Binary(Box::new(lhs.dual()), Infix::Plus, Box::new(rhs.dual()))
                }
                Infix::Times => {
                    Simplified::Binary(Box::new(lhs.dual()), Infix::Par, Box::new(rhs.dual()))
                }
                Infix::Par => {
                    Simplified::Binary(Box::new(lhs.dual()), Infix::Times, Box::new(rhs.dual()))
                }
            },
        }
    }
    /// Remove all lollipops by turning them into pars.
    #[inline]
    #[must_use]
    pub fn funk(self) -> Funky {
        match self {
            Self::Value(name) => Funky::Value(name),
            Self::Dual(name) => Funky::Dual(name),
            Self::Unary(op, arg) => Funky::Unary(op, Box::new(arg.funk())),
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Par => Funky::Binary(
                    Box::new(lhs.funk()),
                    FunkyInfix::Lollipop,
                    Box::new(rhs.dual().funk()),
                ),
                Infix::Plus => {
                    Funky::Binary(Box::new(lhs.funk()), FunkyInfix::Plus, Box::new(rhs.funk()))
                }
                Infix::With => {
                    Funky::Binary(Box::new(lhs.funk()), FunkyInfix::With, Box::new(rhs.funk()))
                }
                Infix::Times => Funky::Binary(
                    Box::new(lhs.funk()),
                    FunkyInfix::Times,
                    Box::new(rhs.funk()),
                ),
            },
        }
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Simplified {
    #[inline]
    #[allow(
        clippy::as_conversions,
        clippy::indexing_slicing,
        clippy::unwrap_used,
        trivial_casts
    )]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        g.choose(
            &[
                (|s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1));
                    let name = Name::arbitrary(&mut r);
                    if bool::arbitrary(&mut r) {
                        Simplified::Value(name)
                    } else {
                        Simplified::Dual(name)
                    }
                }) as fn(usize) -> Self,
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1));
                    Simplified::Unary(Prefix::arbitrary(&mut r), Box::arbitrary(&mut r))
                },
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1) >> 1);
                    Simplified::Binary(
                        Box::arbitrary(&mut r),
                        Infix::arbitrary(&mut r),
                        Box::arbitrary(&mut r),
                    )
                },
            ][..g.size().min(3)],
        )
        .unwrap()(g.size())
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Simplified::Value(c) => Box::new(c.shrink().map(Simplified::Value)),
            &Simplified::Dual(c) => Box::new(
                Simplified::Value(c)
                    .shrink()
                    .chain(c.shrink().map(Simplified::Dual)),
            ),
            &Simplified::Unary(prefix, ref arg) => Box::new(
                arg.as_ref().shrink().chain(
                    (prefix, arg.clone())
                        .shrink()
                        .map(|(p, a)| Simplified::Unary(p, a)),
                ),
            ),
            &Simplified::Binary(ref lhs, infix, ref rhs) => Box::new(
                Simplified::Unary(Prefix::Quest, lhs.clone())
                    .shrink()
                    .chain(Simplified::Unary(Prefix::Quest, rhs.clone()).shrink())
                    .chain(
                        (lhs.clone(), infix, rhs.clone())
                            .shrink()
                            .map(|(l, i, r)| Simplified::Binary(l, i, r)),
                    ),
            ),
        }
    }
}
