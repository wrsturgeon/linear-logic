/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Linear-logic expressions as heap trees.

mod infix;
mod name;
mod nonbinary;
mod prefix;
mod tree;

pub(crate) use nonbinary::Nonbinary;
pub use {
    infix::{Associativity, Infix, PAR},
    name::Name,
    prefix::Prefix,
    tree::{SyntaxAware, Tree},
};
