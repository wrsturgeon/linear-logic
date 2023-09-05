/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Par-less linear-logic expressions as heap trees.

use crate::ast::{
    Atomic, FunkyInfix as Infix, Simplified, SimplifiedInfix, SimplifiedPrefix as Prefix,
    Unsimplified, UnsimplifiedPrefix,
};

/// Par-less linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Funky {
    /// Raw value: e.g. `A`, `0`, bottom, etc.
    Atomic(Atomic),
    /// Dual of a raw value: e.g. `~A`.
    Dual(String),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Funky>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<Funky>, Infix, Box<Funky>),
}

impl From<Funky> for Unsimplified {
    #[inline]
    fn from(value: Funky) -> Self {
        match value {
            Funky::Atomic(atom) => Unsimplified::Atomic(atom),
            Funky::Dual(name) => Unsimplified::Unary(
                UnsimplifiedPrefix::Dual,
                Box::new(Unsimplified::Atomic(Atomic::Bound(name))),
            ),
            Funky::Unary(op, arg) => Unsimplified::Unary(op.into(), Box::new((*arg).into())),
            Funky::Binary(lhs, op, rhs) => {
                Unsimplified::Binary(Box::new((*lhs).into()), op.into(), Box::new((*rhs).into()))
            }
        }
    }
}

impl core::fmt::Display for Funky {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&Unsimplified::from(self.clone()), f)
    }
}

impl Funky {
    /// Compute the dual for this expression without non-atomic duals or pars.
    #[inline]
    #[must_use]
    pub fn dual(self) -> Self {
        match self {
            Self::Atomic(atom) => atom.funky_dual(),
            Self::Dual(name) => Self::Atomic(Atomic::Bound(name)),
            Self::Unary(op, arg) => Self::Unary(op.dual(), Box::new(arg.dual())),
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Lollipop => Simplified::Binary(
                    Box::new(lhs.simplify()),
                    SimplifiedInfix::Par,
                    Box::new(rhs.simplify().dual()),
                )
                .dual()
                .funk(),
                Infix::Plus => {
                    Self::Binary(Box::new(lhs.dual()), Infix::With, Box::new(rhs.dual()))
                }
                Infix::With => {
                    Self::Binary(Box::new(lhs.dual()), Infix::Plus, Box::new(rhs.dual()))
                }
                Infix::Times => Simplified::Binary(
                    Box::new(lhs.simplify().dual()),
                    SimplifiedInfix::Par,
                    Box::new(rhs.simplify().dual()),
                )
                .funk(),
            },
        }
    }
    /// Remove all lollipops by turning them into pars.
    #[inline]
    #[must_use]
    pub fn simplify(self) -> Simplified {
        match self {
            Self::Atomic(atom) => Simplified::Atomic(atom),
            Self::Dual(name) => Simplified::Dual(name),
            Self::Unary(op, arg) => Simplified::Unary(op, Box::new(arg.simplify())),
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Lollipop => Simplified::Binary(
                    Box::new(lhs.simplify()),
                    SimplifiedInfix::Par,
                    Box::new(
                        Unsimplified::Unary(UnsimplifiedPrefix::Dual, Box::new((*rhs).into()))
                            .simplify(),
                    ),
                ),
                Infix::Plus => Simplified::Binary(
                    Box::new(lhs.simplify()),
                    SimplifiedInfix::Plus,
                    Box::new(rhs.simplify()),
                ),
                Infix::With => Simplified::Binary(
                    Box::new(lhs.simplify()),
                    SimplifiedInfix::With,
                    Box::new(rhs.simplify()),
                ),
                Infix::Times => Simplified::Binary(
                    Box::new(lhs.simplify()),
                    SimplifiedInfix::Times,
                    Box::new(rhs.simplify()),
                ),
            },
        }
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Funky {
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
                    let atom = Atomic::arbitrary(&mut r);
                    if bool::arbitrary(&mut r) {
                        Funky::Atomic(atom)
                    } else {
                        let st = String::arbitrary(&mut r);
                        let mut acc = Atomic::Bound(String::new());
                        for c in st.chars() {
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = acc.push::<core::convert::Infallible>(c, usize::MAX);
                        }
                        Funky::Dual(acc.to_string())
                    }
                }) as fn(usize) -> Self,
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(1));
                    Funky::Unary(Prefix::arbitrary(&mut r), Box::arbitrary(&mut r))
                },
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(2) >> 1);
                    Funky::Binary(
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
            &Funky::Atomic(ref atom) => Box::new(atom.shrink().map(Funky::Atomic)),
            &Funky::Dual(ref name) => Box::new(
                Funky::Atomic(Atomic::Top)
                    .shrink()
                    .chain(name.shrink().map(Funky::Dual)),
            ),
            &Funky::Unary(prefix, ref arg) => Box::new(
                arg.as_ref().shrink().chain(
                    (prefix, arg.clone())
                        .shrink()
                        .map(|(p, a)| Funky::Unary(p, a)),
                ),
            ),
            &Funky::Binary(ref lhs, infix, ref rhs) => Box::new(
                Funky::Unary(Prefix::Quest, lhs.clone())
                    .shrink()
                    .chain(Funky::Unary(Prefix::Quest, rhs.clone()).shrink())
                    .chain(
                        (lhs.clone(), infix, rhs.clone())
                            .shrink()
                            .map(|(l, i, r)| Funky::Binary(l, i, r)),
                    ),
            ),
        }
    }
}
