/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Linear-logic expressions as heap trees.

use crate::{
    ast::{Infix, Name, Nonbinary, Prefix},
    parse, Spanned, Triage,
};

/// Either parenthesized or not.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum SyntaxAware {
    /// Raw name: e.g. `A`.
    Value(Name),
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
    #[must_use]
    #[allow(clippy::only_used_in_recursion, clippy::similar_names)]
    pub(crate) fn into_tree(
        self,
        lnbr: Option<Infix>,
        rnbr: Option<Infix>,
    ) -> Triage<Unsimplified, parse::Warning, parse::Error> {
        match self {
            SyntaxAware::Value(v) => Triage::Okay(Unsimplified::Value(v)),
            SyntaxAware::Unary(op, arg) => arg
                .into_unsimplified()
                .map(|a| Unsimplified::Unary(op, Box::new(a))),
            SyntaxAware::Binary(lhs, op, rhs) => lhs.into_tree(lnbr, Some(op)).and_then(|tl| {
                rhs.into_tree(Some(op), rnbr).and_then(|tr| {
                    Triage::Okay(Unsimplified::Binary(Box::new(tl), op, Box::new(tr)))
                })
            }),
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
                lhs.into_tree(None, Some(op)).and_then(|tl| {
                    rhs.into_tree(Some(op), None).and_then(|tr| {
                        Triage::Okay(Unsimplified::Binary(Box::new(tl), op, Box::new(tr)))
                    })
                })
            }),
        }
    }
}

/// Linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Unsimplified {
    /// Raw name: e.g. `A`.
    Value(Name),
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
            &Unsimplified::Value(_) => None,
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
            &Unsimplified::Value(name) => (core::iter::once(char::from(name)).collect(), None),
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

    #[inline]
    #[must_use]
    #[cfg(test)]
    pub fn exhaustive_to_depth(depth: usize) -> Vec<Unsimplified> {
        depth.checked_sub(1).map_or(vec![], |next_depth| {
            let rec = Self::exhaustive_to_depth(next_depth).into_iter();
            core::iter::once(Self::Value(
                #[allow(unsafe_code)]
                // SAFETY:
                // Duh.
                unsafe {
                    Name::from_char('A', 0).strict().unwrap_unchecked()
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
        let mut v = vec![Self::Value(unsafe {
            Name::from_char('A', 0).strict().unwrap_unchecked()
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
                    Unsimplified::Value(Name::arbitrary(&mut quickcheck::Gen::new(
                        s.saturating_sub(1),
                    )))
                }) as fn(usize) -> Self,
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1));
                    Unsimplified::Unary(Prefix::arbitrary(&mut r), Box::arbitrary(&mut r))
                },
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1) >> 1);
                    Unsimplified::Binary(
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
            &Unsimplified::Value(c) => Box::new(c.shrink().map(Unsimplified::Value)),
            &Unsimplified::Unary(prefix, ref arg) => Box::new(
                arg.as_ref().shrink().chain(
                    (prefix, arg.clone())
                        .shrink()
                        .map(|(p, a)| Unsimplified::Unary(p, a)),
                ),
            ),
            &Unsimplified::Binary(ref lhs, infix, ref rhs) => Box::new(
                Unsimplified::Unary(Prefix::Quest, lhs.clone())
                    .shrink()
                    .chain(Unsimplified::Unary(Prefix::Quest, rhs.clone()).shrink())
                    .chain(
                        (lhs.clone(), infix, rhs.clone())
                            .shrink()
                            .map(|(l, i, r)| Unsimplified::Binary(l, i, r)),
                    ),
            ),
        }
    }
}
