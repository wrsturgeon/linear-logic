/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Anything but a binary operation (e.g. not `A * B`).

use crate::{
    ast::{
        unsimplified::SyntaxAware, Atomic, Unsimplified, UnsimplifiedInfix,
        UnsimplifiedPrefix as Prefix,
    },
    parse, Triage,
};

/// Anything but a binary operation (e.g. not `A * B`).
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum Nonbinary {
    /// Raw value without operations: e.g. `A`.
    Atomic(Atomic),
    /// Unary operation: e.g. `?A`.
    Unary(Prefix, Box<Nonbinary>),
    /// Binary operation: e.g. `A * B`.
    Parenthesized(Box<SyntaxAware>, UnsimplifiedInfix, Box<SyntaxAware>, usize),
}

impl Nonbinary {
    /// After a unary operation while converting to a tree.
    #[inline]
    #[allow(clippy::only_used_in_recursion, clippy::similar_names)]
    pub(crate) fn into_unsimplified(self) -> Triage<Unsimplified, parse::Warning, parse::Error> {
        match self {
            Nonbinary::Atomic(v) => Triage::Okay(Unsimplified::Atomic(v)),
            Nonbinary::Unary(op, arg) => arg
                .into_unsimplified()
                .map(|a| Unsimplified::Unary(op, Box::new(a))),
            Nonbinary::Parenthesized(lhs, op, rhs, _) => {
                lhs.into_unsimplified(None, Some(op)).and_then(|tl| {
                    rhs.into_unsimplified(Some(op), None).and_then(|tr| {
                        Triage::Okay(Unsimplified::Binary(Box::new(tl), op, Box::new(tr)))
                    })
                })
            }
        }
    }

    /// Check if we might still be reading a name.
    /// If we are, return a mutable reference to it.
    #[inline]
    pub(crate) fn reading_name(&mut self) -> Option<&mut Atomic> {
        match self {
            &mut Self::Atomic(ref mut atom) => atom.reading_name(),
            &mut Self::Unary(_, ref mut nb) => nb.reading_name(),
            &mut Self::Parenthesized(..) => None,
        }
    }
}

impl From<Nonbinary> for SyntaxAware {
    #[inline]
    fn from(value: Nonbinary) -> Self {
        match value {
            Nonbinary::Atomic(c) => Self::Atomic(c),
            Nonbinary::Unary(op, arg) => Self::Unary(op, arg),
            Nonbinary::Parenthesized(lhs, op, rhs, index) => {
                Self::Parenthesized(lhs, op, rhs, index)
            }
        }
    }
}
