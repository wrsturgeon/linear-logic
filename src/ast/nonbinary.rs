/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Anything but a binary operation (e.g. not `A * B`).

use crate::ast::{Name, Prefix, SyntaxAware};

/// Anything but a binary operation (e.g. not `A * B`).
pub(crate) enum Nonbinary {
    /// Raw value without operations, e.g. `A`.
    Value(Name),
    /// Unary operation, e.g. `?A`.
    Unary(Prefix, Box<Nonbinary>),
    /// Binary operation, e.g. `A * B`.
    Parenthesized(Box<SyntaxAware>),
}

impl From<Nonbinary> for SyntaxAware {
    #[inline(always)]
    fn from(value: Nonbinary) -> Self {
        match value {
            Nonbinary::Value(c) => Self::Value(c),
            Nonbinary::Unary(op, arg) => Self::Unary(op, Box::new((*arg).into())),
            Nonbinary::Parenthesized(tree) => Self::Parenthesized(tree),
        }
    }
}
