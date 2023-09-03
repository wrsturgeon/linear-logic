/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Linear-logic expressions as heap trees.

use crate::{
    ast::{Infix, Name, Nonbinary, Prefix},
    parse, Triage,
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
    Parenthesized(Box<SyntaxAware>, Infix, Box<SyntaxAware>),
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
    ) -> Triage<Tree, parse::Warning, parse::Error> {
        #[allow(clippy::wildcard_enum_match_arm)] // FIXME: remove
        match self {
            SyntaxAware::Value(v) => Triage::Okay(Tree::Value(v)),
            SyntaxAware::Unary(op, arg) => arg.into_tree().map(|a| Tree::Unary(op, Box::new(a))),
            SyntaxAware::Binary(lhs, op, rhs) => lhs.into_tree(lnbr, Some(op)).and_then(|tl| {
                rhs.into_tree(Some(op), rnbr)
                    .and_then(|tr| Triage::Okay(Tree::Binary(Box::new(tl), op, Box::new(tr))))
            }),
            SyntaxAware::Parenthesized(lhs, op, rhs) => if lnbr
                .map_or(false, |other| op.weaker_to_the_right_of(other))
                || rnbr.map_or(false, |other| op.weaker_to_the_left_of(other))
            {
                Triage::Okay(())
            } else {
                Triage::Warn((), parse::Warning::UnnecessaryParens)
            }
            .and_then(|()| {
                lhs.into_tree(None, None).and_then(|tl| {
                    rhs.into_tree(None, None)
                        .and_then(|tr| Triage::Okay(Tree::Binary(Box::new(tl), op, Box::new(tr))))
                })
            }),
        }
    }
}

/// Linear-logic expressions as heap trees.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Tree {
    /// Raw name: e.g. `A`.
    Value(Name),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Tree>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<Tree>, Infix, Box<Tree>),
}

impl Tree {
    /// Get the topmost binary operation.
    #[inline]
    #[must_use]
    pub const fn top_bin_op(&self) -> Option<Infix> {
        match self {
            &Tree::Value(_) => None,
            &Tree::Unary(_, ref arg) => arg.top_bin_op(),
            &Tree::Binary(_, op, _) => Some(op),
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
            &Tree::Value(name) => (core::iter::once(char::from(name)).collect(), None),
            &Tree::Unary(op, ref arg) => {
                if let &Tree::Binary(_, _, _) = arg.as_ref() {
                    (
                        op.to_string() + "(" + &arg.display(None, None).0 + ")",
                        None,
                    )
                } else {
                    (op.to_string() + &arg.display(None, None).0, None)
                }
            }
            &Tree::Binary(ref lhs, op, ref rhs) => {
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
    pub fn exhaustive_up_to_depth(depth: usize) -> Vec<Self> {
        depth.checked_sub(1).map_or(Vec::new(), |rec| {
            #[allow(unsafe_code)]
            // SAFETY:
            // Duh.
            let mut v = vec![Self::Value(unsafe {
                Name::from_char('A').strict().unwrap_unchecked()
            })];
            for t in Self::exhaustive_up_to_depth(rec) {
                v.push(Self::Unary(Prefix::Bang, Box::new(t)));
            }
            for lhs in Self::exhaustive_up_to_depth(rec) {
                for rhs in Self::exhaustive_up_to_depth(rec) {
                    v.push(Self::Binary(
                        Box::new(lhs.clone()),
                        Infix::Times,
                        Box::new(rhs),
                    ));
                }
            }
            v
        })
    }
}

impl core::fmt::Display for Tree {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.display(None, None).0)
    }
}

#[cfg(feature = "quickcheck")]
impl quickcheck::Arbitrary for Tree {
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
                    Tree::Value(Name::arbitrary(&mut quickcheck::Gen::new(
                        s.saturating_sub(1),
                    )))
                }) as fn(usize) -> Self,
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1));
                    Tree::Unary(Prefix::arbitrary(&mut r), Box::arbitrary(&mut r))
                },
                |s| {
                    let mut r = quickcheck::Gen::new(s.saturating_sub(1) >> 1);
                    Tree::Binary(
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
            &Tree::Value(c) => Box::new(c.shrink().map(Tree::Value)),
            &Tree::Unary(prefix, ref arg) => Box::new(
                arg.as_ref().shrink().chain(
                    (prefix, arg.clone())
                        .shrink()
                        .map(|(p, a)| Tree::Unary(p, a)),
                ),
            ),
            &Tree::Binary(ref lhs, infix, ref rhs) => Box::new(
                Tree::Unary(Prefix::Quest, lhs.clone())
                    .shrink()
                    .chain(Tree::Unary(Prefix::Quest, rhs.clone()).shrink())
                    .chain(
                        (lhs.clone(), infix, rhs.clone())
                            .shrink()
                            .map(|(l, i, r)| Tree::Binary(l, i, r)),
                    ),
            ),
        }
    }
}
