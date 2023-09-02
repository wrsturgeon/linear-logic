/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Linear-logic expressions as heap trees.

use crate::ast::{Infix, Name, Prefix};

/// Either parenthesized or not.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SyntaxAware {
    /// Raw name: e.g. `A`.
    Value(Name),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Tree>),
    /// Binary operation: e.g. `A * B`.
    Binary(Box<Tree>, Infix, Box<SyntaxAware>),
    /// Parenthesized tree.
    Parenthesized(Box<Tree>),
}

impl From<SyntaxAware> for Tree {
    #[inline]
    fn from(value: SyntaxAware) -> Self {
        match value {
            SyntaxAware::Value(v) => Tree::Value(v),
            SyntaxAware::Unary(op, arg) => Tree::Unary(op, arg),
            SyntaxAware::Binary(lhs, op, rhs) => Tree::Binary(lhs, op, Box::new((*rhs).into())),
            SyntaxAware::Parenthesized(tree) => *tree,
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

impl core::fmt::Display for Tree {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            &Tree::Value(ref name) => write!(f, "{name}"),
            &Tree::Unary(ref op, ref tree) => write!(f, "{op}{tree}"),
            &Tree::Binary(ref lhs, ref op, ref rhs) => write!(f, "({lhs} {op} {rhs})"),
        }
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
