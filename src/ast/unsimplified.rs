/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Unsimplified linear-logic expressions as heap trees.

use crate::{
    ast::{
        Atomic, Funky, FunkyInfix, Nonbinary, Simplified, SimplifiedInfix, SimplifiedPrefix,
        UnsimplifiedInfix as Infix, UnsimplifiedPrefix as Prefix,
    },
    parse, Spanned, Triage,
};

/// Either parenthesized or not.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum SyntaxAware {
    /// Raw value: e.g. `A`, `0`, bottom, etc.
    Atomic(Atomic),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Nonbinary>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<SyntaxAware>, Infix, Box<SyntaxAware>),
    /// Parenthesized tree.
    Parenthesized(Box<SyntaxAware>, Infix, Box<SyntaxAware>, usize),
}

impl SyntaxAware {
    /// Convert to a tree by removing parentheses and warn if parentheses were unnecessary.
    #[inline]
    #[allow(clippy::only_used_in_recursion, clippy::similar_names)]
    pub(crate) fn into_unsimplified(
        self,
        lnbr: Option<Infix>,
        rnbr: Option<Infix>,
    ) -> Triage<Unsimplified, parse::Warning, parse::Error> {
        match self {
            SyntaxAware::Atomic(v) => Triage::Okay(Unsimplified::Atomic(v)),
            SyntaxAware::Unary(op, arg) => arg
                .into_unsimplified()
                .map(|a| Unsimplified::Unary(op, Box::new(a))),
            SyntaxAware::Binary(lhs, op, rhs) => {
                lhs.into_unsimplified(lnbr, Some(op)).and_then(|tl| {
                    rhs.into_unsimplified(Some(op), rnbr).and_then(|tr| {
                        Triage::Okay(Unsimplified::Binary(Box::new(tl), op, Box::new(tr)))
                    })
                })
            }
            SyntaxAware::Parenthesized(lhs, op, rhs, index) => if lnbr
                .map_or(false, |other| op.weaker_to_the_right_of(other))
                || rnbr.map_or(false, |other| op.weaker_to_the_left_of(other))
            {
                Triage::Okay(())
            } else {
                Triage::Warn(
                    (),
                    Spanned {
                        msg: parse::Warning::UnnecessaryParens,
                        index,
                    },
                )
            }
            .and_then(|()| {
                lhs.into_unsimplified(None, Some(op)).and_then(|tl| {
                    rhs.into_unsimplified(Some(op), None).and_then(|tr| {
                        Triage::Okay(Unsimplified::Binary(Box::new(tl), op, Box::new(tr)))
                    })
                })
            }),
        }
    }

    /// Check if we might still be reading a name.
    /// If we are, return a mutable reference to it.
    #[inline]
    pub(crate) fn reading_name(&mut self) -> Option<&mut Atomic> {
        match self {
            &mut Self::Atomic(ref mut atom) => atom.reading_name(),
            &mut Self::Unary(_, ref mut nb) => nb.reading_name(),
            &mut Self::Binary(_, _, ref mut rhs) => rhs.reading_name(),
            &mut Self::Parenthesized(..) => None,
        }
    }
}

/// Unsimplified linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Unsimplified {
    /// Raw atom: e.g. `A`.
    Atomic(Atomic),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Unsimplified>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<Unsimplified>, Infix, Box<Unsimplified>),
}

impl Unsimplified {
    /// Get the topmost binary operation.
    #[inline]
    #[must_use]
    pub const fn top_bin_op(&self) -> Option<Infix> {
        match self {
            &Unsimplified::Atomic(_) => None,
            &Unsimplified::Unary(_, ref arg) => arg.top_bin_op(),
            &Unsimplified::Binary(_, op, _) => Some(op),
        }
    }
    /// Display with knowledge of our neighbors to inform parenthesization.
    #[inline]
    #[must_use]
    #[allow(
        clippy::arithmetic_side_effects,
        clippy::only_used_in_recursion,
        clippy::similar_names
    )]
    pub fn display(&self, lnbr: Option<Infix>, rnbr: Option<Infix>) -> (String, Option<Infix>) {
        match self {
            &Unsimplified::Atomic(ref atomic) => (atomic.to_string(), None),
            &Unsimplified::Unary(op, ref arg) => {
                if let &Unsimplified::Binary(_, _, _) = arg.as_ref() {
                    (
                        op.to_string() + "(" + &arg.display(None, None).0 + ")",
                        None,
                    )
                } else {
                    (op.to_string() + &arg.display(None, None).0, None)
                }
            }
            &Unsimplified::Binary(ref lhs, op, ref rhs) => {
                // We should never surround ourselves in parentheses, only our left- and right-children.
                // This way the top level is never parenthesized pointlessly (and this probably makes sense recursively).
                let mut s = String::new();
                {
                    let (sl, ol) = lhs.display(lnbr, Some(op));
                    let parenthesize = ol.map_or(false, |other| other.weaker_to_the_left_of(op));
                    if parenthesize {
                        s.push('(');
                    }
                    s.push_str(&sl);
                    if parenthesize {
                        s.push(')');
                    }
                }
                s.push(' ');
                s.push_str(&op.to_string());
                s.push(' ');
                {
                    let (sr, or) = rhs.display(Some(op), rnbr);
                    let parenthesize = or.map_or(false, |other| other.weaker_to_the_right_of(op));
                    if parenthesize {
                        s.push('(');
                    }
                    s.push_str(&sr);
                    if parenthesize {
                        s.push(')');
                    }
                }
                (s, Some(op))
            }
        }
    }

    /// Compute the dual for this expression without non-atomic duals or lollipops.
    #[inline]
    #[must_use]
    fn simplified_dual(self) -> Simplified {
        match self {
            Self::Atomic(atom) => atom.simplified_dual(),
            Self::Unary(op, arg) => match op {
                Prefix::Dual => arg.simplify(),
                Prefix::Bang => {
                    Simplified::Unary(SimplifiedPrefix::Quest, Box::new(arg.simplified_dual()))
                }
                Prefix::Quest => {
                    Simplified::Unary(SimplifiedPrefix::Bang, Box::new(arg.simplified_dual()))
                }
            },
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Lollipop => {
                    Self::Binary(Box::new(Self::Unary(Prefix::Dual, lhs)), Infix::Par, rhs)
                        .simplified_dual()
                }
                Infix::Plus => Simplified::Binary(
                    Box::new(lhs.simplified_dual()),
                    SimplifiedInfix::With,
                    Box::new(rhs.simplified_dual()),
                ),
                Infix::With => Simplified::Binary(
                    Box::new(lhs.simplified_dual()),
                    SimplifiedInfix::Plus,
                    Box::new(rhs.simplified_dual()),
                ),
                Infix::Times => Simplified::Binary(
                    Box::new(lhs.simplified_dual()),
                    SimplifiedInfix::Par,
                    Box::new(rhs.simplified_dual()),
                ),
                Infix::Par => Simplified::Binary(
                    Box::new(lhs.simplified_dual()),
                    SimplifiedInfix::Times,
                    Box::new(rhs.simplified_dual()),
                ),
            },
        }
    }

    /// Simplify a linear-logic expression to remove all non-atomic duals and lollipops.
    #[inline]
    #[must_use]
    pub fn simplify(self) -> Simplified {
        match self {
            Self::Atomic(atom) => Simplified::Atomic(atom),
            Self::Unary(op, arg) => match op {
                Prefix::Bang => Simplified::Unary(SimplifiedPrefix::Bang, Box::new(arg.simplify())),
                Prefix::Quest => {
                    Simplified::Unary(SimplifiedPrefix::Quest, Box::new(arg.simplify()))
                }
                Prefix::Dual => arg.simplified_dual(),
            },
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Lollipop => {
                    Self::Binary(Box::new(Self::Unary(Prefix::Dual, lhs)), Infix::Par, rhs)
                        .simplify()
                }
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
                Infix::Par => Simplified::Binary(
                    Box::new(lhs.simplify()),
                    SimplifiedInfix::Par,
                    Box::new(rhs.simplify()),
                ),
            },
        }
    }

    /// Compute the dual for this expression without non-atomic duals or lollipops.
    #[inline]
    #[must_use]
    fn funky_dual(self) -> Funky {
        match self {
            Self::Atomic(atom) => atom.funky_dual(),
            Self::Unary(op, arg) => match op {
                Prefix::Dual => arg.funk(),
                Prefix::Bang => Funky::Unary(SimplifiedPrefix::Quest, Box::new(arg.funky_dual())),
                Prefix::Quest => Funky::Unary(SimplifiedPrefix::Bang, Box::new(arg.funky_dual())),
            },
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Lollipop => {
                    Self::Binary(Box::new(Self::Unary(Prefix::Dual, lhs)), Infix::Par, rhs)
                        .funky_dual()
                }
                Infix::Plus => Funky::Binary(
                    Box::new(lhs.funky_dual()),
                    FunkyInfix::With,
                    Box::new(rhs.funky_dual()),
                ),
                Infix::With => Funky::Binary(
                    Box::new(lhs.funky_dual()),
                    FunkyInfix::Plus,
                    Box::new(rhs.funky_dual()),
                ),
                Infix::Times => Self::Binary(
                    Box::new(Self::Unary(Prefix::Dual, lhs)),
                    Infix::Par,
                    Box::new(Self::Unary(Prefix::Dual, rhs)),
                )
                .funk(),
                Infix::Par => Funky::Binary(
                    Box::new(lhs.funky_dual()),
                    FunkyInfix::Times,
                    Box::new(rhs.funky_dual()),
                ),
            },
        }
    }

    /// Simplify a linear-logic expression to remove all non-atomic duals and par operators.
    #[inline]
    #[must_use]
    pub fn funk(self) -> Funky {
        match self {
            Self::Atomic(atom) => Funky::Atomic(atom),
            Self::Unary(op, arg) => match op {
                Prefix::Bang => Funky::Unary(SimplifiedPrefix::Bang, Box::new(arg.funk())),
                Prefix::Quest => Funky::Unary(SimplifiedPrefix::Quest, Box::new(arg.funk())),
                Prefix::Dual => arg.funky_dual(),
            },
            Self::Binary(lhs, op, rhs) => match op {
                Infix::Lollipop => {
                    Self::Binary(Box::new(Self::Unary(Prefix::Dual, lhs)), Infix::Par, rhs).funk()
                }
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
                Infix::Par => Funky::Binary(
                    Box::new(lhs.funky_dual()),
                    FunkyInfix::Lollipop,
                    Box::new(rhs.funk()),
                ),
            },
        }
    }

    #[inline]
    #[must_use]
    #[cfg(test)]
    pub fn exhaustive_to_depth(depth: usize) -> Vec<Self> {
        depth.checked_sub(1).map_or(vec![], |next_depth| {
            let rec = Self::exhaustive_to_depth(next_depth).into_iter();
            core::iter::once(Self::Atomic(
                #[allow(unsafe_code)]
                // SAFETY:
                // Duh.
                unsafe {
                    Atomic::from_char('A', usize::MAX)
                        .strict()
                        .unwrap_unchecked()
                },
            ))
            .chain(rec.clone().map(|t| Self::Unary(Prefix::Bang, Box::new(t))))
            .chain(rec.clone().flat_map(move |lhs| {
                rec.clone().map(move |rhs| {
                    Self::Binary(Box::new(lhs.clone()), Infix::Times, Box::new(rhs))
                })
            }))
            .collect()
        })
    }

    #[inline]
    #[must_use]
    #[cfg(test)]
    pub fn exhaustive_to_length(len: usize, paren: bool) -> Vec<Unsimplified> {
        if len == 0 {
            return vec![];
        }
        #[allow(unsafe_code)]
        // SAFETY:
        // Duh.
        let mut v = vec![Self::Atomic(unsafe {
            Atomic::from_char('a', 0).strict().unwrap_unchecked()
        })];
        for t in Self::exhaustive_to_length(
            match len.saturating_sub(1) {
                0 => return v,
                i => i,
            },
            true,
        ) {
            v.push(Self::Unary(Prefix::Bang, Box::new(t)));
        }
        let post_bin = match len.saturating_sub(if paren { 5 } else { 3 }) {
            0 => return v,
            i => i,
        };
        for lhs in Self::exhaustive_to_length(post_bin, false) {
            for rhs in
                Self::exhaustive_to_length(post_bin.saturating_sub(format!("{lhs}").len()), false)
            {
                v.push(Self::Binary(
                    Box::new(lhs.clone()),
                    Infix::Times,
                    Box::new(rhs),
                ));
            }
        }
        v
    }

    /// Mutate leaves (names) with some function.
    #[inline]
    #[must_use]
    pub fn map<F: Fn(String) -> String>(self, f: &F) -> Self {
        match self {
            Self::Atomic(atom) => Self::Atomic(atom.map(f)),
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
            &mut Self::Unary(_, ref mut nb) => nb.names(),
            &mut Self::Binary(ref mut lhs, _, ref mut rhs) => {
                let mut v = lhs.names();
                v.append(&mut rhs.names());
                v
            }
        }
    }
}

impl core::fmt::Display for Unsimplified {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.display(None, None).0)
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Unsimplified {
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
                    Unsimplified::Atomic(Atomic::arbitrary(&mut quickcheck::Gen::new(
                        s.saturating_sub(1).max(1),
                    )))
                }) as fn(usize) -> Self,
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(1));
                    Unsimplified::Unary(Prefix::arbitrary(&mut r), Box::arbitrary(&mut r))
                },
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1).max(2) >> 1);
                    Unsimplified::Binary(
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
            &Unsimplified::Atomic(ref atom) => Box::new(atom.shrink().map(Unsimplified::Atomic)),
            &Unsimplified::Unary(prefix, ref arg) => Box::new(
                arg.as_ref().shrink().chain(
                    (prefix, arg.clone())
                        .shrink()
                        .map(|(p, a)| Unsimplified::Unary(p, a)),
                ),
            ),
            &Unsimplified::Binary(ref lhs, infix, ref rhs) => Box::new(
                Unsimplified::Unary(Prefix::Dual, lhs.clone())
                    .shrink()
                    .chain(Unsimplified::Unary(Prefix::Dual, rhs.clone()).shrink())
                    .chain(
                        (lhs.clone(), infix, rhs.clone())
                            .shrink()
                            .map(|(l, i, r)| Unsimplified::Binary(l, i, r)),
                    ),
            ),
        }
    }
}
