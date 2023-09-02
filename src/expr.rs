/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Linear-logic expressions as heap trees.

// pub enum Prefix {
//     Bang,
//     Quest,
// }

// pub struct Prefixed {
//     name: u8,
//     prefixes: Vec<Prefix>,
// }

// pub enum Infix {
//     Times,
//     Plus,
//     With,
//     Par,
// }

// pub struct Flattened {
//     head: Prefixed,
//     tail: Vec<(Infix, Prefixed)>,
// }

/// Linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expr {
    /// A raw name: e.g. `A`.
    Name(u8),
    /// The "of course" operator, read as "bang."
    Bang(Box<Expr>),
    /// The "why not" operator, read as "quest."
    Quest(Box<Expr>),
    /// Multiplicative conjunction, read as "times" or "tensor."
    Times(Box<Expr>, Box<Expr>),
    /// Additive disjunction, read as "plus."
    Plus(Box<Expr>, Box<Expr>),
    /// Additive conjunction, read as "with."
    With(Box<Expr>, Box<Expr>),
    /// Multiplicative disjunction, read as "par."
    Par(Box<Expr>, Box<Expr>),
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Expr {
    #[inline]
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let triple = u8::arbitrary(g) % 3;
        #[allow(clippy::arithmetic_side_effects)]
        match u8::try_from(g.size()).map_or(triple, |s| if s == 0 { 0 } else { triple % s }) {
            0 => loop {
                let c = u8::arbitrary(g);
                if c.is_ascii_uppercase() {
                    return Expr::Name(c);
                }
            },
            1 => {
                #[allow(clippy::as_conversions, trivial_casts)]
                let always_gonna_be_some = g.choose(&[
                    (|mut r| Expr::Bang(quickcheck::Arbitrary::arbitrary(&mut r))) as fn(_) -> _,
                    (|mut r| Expr::Quest(quickcheck::Arbitrary::arbitrary(&mut r))),
                ]);
                #[allow(clippy::unwrap_used)]
                always_gonna_be_some.unwrap()(quickcheck::Gen::new(g.size().saturating_sub(1)))
            }
            2 => {
                #[allow(clippy::as_conversions, trivial_casts)]
                let always_gonna_be_some = g.choose(&[
                    (|mut r| {
                        Expr::Times(
                            quickcheck::Arbitrary::arbitrary(&mut r),
                            quickcheck::Arbitrary::arbitrary(&mut r),
                        )
                    }) as fn(_) -> _,
                    |mut r| {
                        Expr::Plus(
                            quickcheck::Arbitrary::arbitrary(&mut r),
                            quickcheck::Arbitrary::arbitrary(&mut r),
                        )
                    },
                    |mut r| {
                        Expr::With(
                            quickcheck::Arbitrary::arbitrary(&mut r),
                            quickcheck::Arbitrary::arbitrary(&mut r),
                        )
                    },
                    |mut r| {
                        Expr::Par(
                            quickcheck::Arbitrary::arbitrary(&mut r),
                            quickcheck::Arbitrary::arbitrary(&mut r),
                        )
                    },
                ]);
                #[allow(clippy::unwrap_used)]
                always_gonna_be_some.unwrap()(quickcheck::Gen::new(g.size().saturating_sub(1) >> 1))
            }
            _ => {
                #[cfg(test)]
                #[allow(clippy::unreachable)]
                {
                    unreachable!()
                }
                #[cfg(not(test))]
                #[allow(unsafe_code)]
                // SAFETY:
                // Nothing `% 3` can equal or exceed 3.
                unsafe {
                    core::hint::unreachable_unchecked()
                }
            }
        }
    }
    #[inline]
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            &Expr::Name(c) => Box::new((b'A'..c).map(Expr::Name)),
            &Expr::Bang(ref expr) => {
                Box::new(expr.as_ref().shrink().chain(expr.shrink().map(Expr::Bang)))
            }
            &Expr::Quest(ref expr) => {
                Box::new(expr.as_ref().shrink().chain(expr.shrink().map(Expr::Quest)))
            }
            &Expr::Times(ref a, ref b) => Box::new({
                a.as_ref().shrink().chain(b.as_ref().shrink()).chain(
                    (a.clone(), b.clone())
                        .shrink()
                        .map(|(aa, bb)| Expr::Times(aa, bb)),
                )
            }),
            &Expr::Plus(ref a, ref b) => Box::new({
                a.as_ref()
                    .shrink()
                    .chain(b.as_ref().shrink())
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Times(aa, bb)),
                    )
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Plus(aa, bb)),
                    )
            }),
            &Expr::With(ref a, ref b) => Box::new({
                a.as_ref()
                    .shrink()
                    .chain(b.as_ref().shrink())
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Times(aa, bb)),
                    )
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Plus(aa, bb)),
                    )
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::With(aa, bb)),
                    )
            }),
            &Expr::Par(ref a, ref b) => Box::new({
                a.as_ref()
                    .shrink()
                    .chain(b.as_ref().shrink())
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Times(aa, bb)),
                    )
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Plus(aa, bb)),
                    )
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::With(aa, bb)),
                    )
                    .chain(
                        (a.clone(), b.clone())
                            .shrink()
                            .map(|(aa, bb)| Expr::Par(aa, bb)),
                    )
            }),
        }
    }
}

impl core::fmt::Display for Expr {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            &Expr::Name(name) => char::from(name).fmt(f),
            &Expr::Times(ref a, ref b) => write!(f, "{a} * {b}"),
            &Expr::Plus(ref a, ref b) => write!(f, "{a} + {b}"),
            &Expr::With(ref a, ref b) => write!(f, "{a} & {b}"),
            &Expr::Par(ref a, ref b) => write!(f, "{a} {} {b}", crate::ast::PAR),
            &Expr::Bang(ref expr) => write!(f, "!{expr}"),
            &Expr::Quest(ref expr) => write!(f, "?{expr}"),
        }
    }
}
