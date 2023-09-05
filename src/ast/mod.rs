/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Linear-logic expressions as heap trees.

mod atomic;
mod funky;
mod infix;
mod nonbinary;
mod prefix;
mod simplified;
pub(crate) mod unsimplified;

pub(crate) use nonbinary::Nonbinary;
pub use {
    atomic::Atomic,
    funky::Funky,
    infix::{Associativity, FunkyInfix, SimplifiedInfix, UnsimplifiedInfix, PAR, PAR_STR},
    prefix::{SimplifiedPrefix, UnsimplifiedPrefix},
    simplified::Simplified,
    unsimplified::Unsimplified,
};
