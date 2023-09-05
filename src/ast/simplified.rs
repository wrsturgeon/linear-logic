/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Simplified linear-logic expressions as heap trees.

use crate::ast::{
    Atomic, Funky, FunkyInfix, SimplifiedInfix as Infix, SimplifiedPrefix as Prefix, Unsimplified,
    UnsimplifiedPrefix,
};

/// Simplified linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Simplified {
    /// Raw value: e.g. `A`, `0`, bottom, etc.
    Atomic(Atomic),
    /// Dual of a raw value: e.g. `~A`.
    Dual(String),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Simplified>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<Simplified>, Infix, Box<Simplified>),
}

impl From<Simplified> for Unsimplified {
    #[inline]
    fn from(value: Simplified) -> Self {
        match value {
            Simplified::Atomic(atom) => Unsimplified::Atomic(atom),
            Simplified::Dual(name) => Unsimplified::Unary(
                UnsimplifiedPrefix::Dual,
                Box::new(Unsimplified::Atomic(Atomic::Bound(name))),
            ),
            Simplified::Unary(op, arg) => Unsimplified::Unary(op.into(), Box::new((*arg).into())),
            Simplified::Binary(lhs, op, rhs) => {
                Unsimplified::Binary(Box::new((*lhs).into()), op.into(), Box::new((*rhs).into()))
            }
        }
    }
}

impl core::fmt::Display for Simplified {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&Unsimplified::from(self.clone()), f)
    }
}

impl Simplified {
    /// Compute the dual for this expression without non-atomic duals or lollipops.
    #[inline]
    #[must_use]
    pub fn dual(self) -> Self {
        match self {
            Self::Atomic(atom) => atom.simplified_dual(),
            Self::Dual(name) => Self::Atomic(Atomic::Bound(name)),
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
            Self::Atomic(atom) => Funky::Atomic(atom),
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

    /// Mutate leaves (names) with some function.
    #[inline]
    #[must_use]
    pub fn map<F: Fn(String) -> String>(self, f: &F) -> Self {
        match self {
            Self::Atomic(atom) => Self::Atomic(atom.map(f)),
            Self::Dual(name) => Self::Dual(f(name)),
            Self::Unary(op, nb) => Self::Unary(op, Box::new(nb.map(f))),
            Self::Binary(lhs, op, rhs) => {
                Self::Binary(Box::new(lhs.map(f)), op, Box::new(rhs.map(f)))
            }
        }
    }

    /// Mutable references to all names.
    #[inline]
    pub fn names(&mut self) -> Vec<&mut String> {
        match self {
            &mut Self::Atomic(ref mut atom) => atom.names(),
            &mut Self::Dual(ref mut name) => vec![name],
            &mut Self::Unary(_, ref mut nb) => nb.names(),
            &mut Self::Binary(ref mut lhs, _, ref mut rhs) => {
                let mut v = lhs.names();
                v.append(&mut rhs.names());
                v
            }
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
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(1));
                    if bool::arbitrary(&mut r) {
                        Simplified::Atomic(Atomic::arbitrary(&mut r))
                    } else {
                        let st = String::arbitrary(&mut r);
                        let mut acc = Atomic::Bound(String::new());
                        for c in st.chars() {
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = acc.push::<core::convert::Infallible>(c, usize::MAX);
                        }
                        Simplified::Dual(acc.to_string())
                    }
                }) as fn(usize) -> Self,
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(1));
                    Simplified::Unary(Prefix::arbitrary(&mut r), Box::arbitrary(&mut r))
                },
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(2) >> 1);
                    Simplified::Binary(
                        Box::arbitrary(&mut r),
                        Infix::arbitrary(&mut r),
                        Box::arbitrary(&mut r),
                    )
                },
            ][..g.size().clamp(1, 3)],
        )
        .unwrap()(g.size())
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Simplified::Atomic(ref atom) => Box::new(atom.shrink().map(Simplified::Atomic)),
            &Simplified::Dual(ref name) => Box::new(
                Simplified::Atomic(Atomic::Top)
                    .shrink()
                    .chain(name.shrink().map(Simplified::Dual)),
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
