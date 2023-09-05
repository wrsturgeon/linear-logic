/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Anything but a binary operation (e.g. not `A * B`).

use crate::{
    ast::{unsimplified::SyntaxAware, Infix, Name, Prefix, Unsimplified},
    parse, Triage,
};

/// Anything but a binary operation (e.g. not `A * B`).
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum Nonbinary {
    /// Raw value without operations: e.g. `A`.
    Value(Name),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Nonbinary>),
    /// Binary operation: e.g. `A * B`.
    Parenthesized(Box<SyntaxAware>, Infix, Box<SyntaxAware>, usize),
}

impl Nonbinary {
    /// After a unary operation while converting to a tree.
    #[inline]
    #[must_use]
    #[allow(clippy::only_used_in_recursion, clippy::similar_names)]
    pub(crate) fn into_unsimplified(self) -> Triage<Unsimplified, parse::Warning, parse::Error> {
        match self {
            Nonbinary::Value(v) => Triage::Okay(Unsimplified::Value(v)),
            Nonbinary::Unary(op, arg) => arg
                .into_unsimplified()
                .map(|a| Unsimplified::Unary(op, Box::new(a))),
            Nonbinary::Parenthesized(lhs, op, rhs, _) => {
                lhs.into_tree(None, Some(op)).and_then(|tl| {
                    rhs.into_tree(Some(op), None).and_then(|tr| {
                        Triage::Okay(Unsimplified::Binary(Box::new(tl), op, Box::new(tr)))
                    })
                })
            }
        }
    }
}

impl From<Nonbinary> for SyntaxAware {
    #[inline]
    fn from(value: Nonbinary) -> Self {
        match value {
            Nonbinary::Value(c) => Self::Value(c),
            Nonbinary::Unary(op, arg) => Self::Unary(op, arg),
            Nonbinary::Parenthesized(lhs, op, rhs, index) => {
                Self::Parenthesized(lhs, op, rhs, index)
            }
        }
    }
}
