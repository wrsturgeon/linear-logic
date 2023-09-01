/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Parsing linear-logic expressions.

use crate::{
    ast::{self, Infix, Name, Nonbinary, Prefix, SyntaxAware, Tree, PAR},
    OptionTriage, Triage,
};

/// Any non-fatal warning that could occur parsing a linear-logic expression.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Warning {}

/// Any fatal error that could occur parsing a linear-logic expression.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Error {
    /// Empty expression, e.g. "" or "()".
    EmptyExpression,
    /// Iterator ended but we needed more input.
    End,
    /// Unrecognized infix operator.
    UnrecognizedInfixOperator(char),
    /// Unrecognized prefix operator.
    UnrecognizedPrefix(char),
    /// Used an infix operator without an argument to its left.
    InfixWithoutLeftOperand(u8),
    /// Used an infix operator without an argument to its right.
    InfixWithoutRightOperand,
    /// Missing a left (opening) parenthesis.
    MissingLeftParen,
    /// Missing a right (closing) parenthesis.
    MissingRightParen,
    /// Invalid name.
    InvalidName(char),
}

// TODO:
// impl core::fmt::Display for Error {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::UnrecognizedPrefix(c, v) => {
//                 write!(
//                     f,
//                     "Unrecognized prefix operator '{}'. Rest of the input: `{v:#?}`",
//                     c.escape_default(),
//                 )
//             }
//         }
//     }
// }

/// Parse a linear-logic expression from an iterator over anything that can be converted to characters.
#[inline]
pub fn parse<I: IntoIterator>(i: I) -> Triage<Tree, Warning, Error>
where
    I::Item: Into<char>,
{
    start(&mut i.into_iter(), false).map(Into::into)
}

/// Parse a linear-logic expression from the beginning.
#[inline]
fn start<I: Iterator>(i: &mut I, nested: bool) -> Triage<SyntaxAware, Warning, Error>
where
    I::Item: Into<char>,
{
    i.next().triage(Error::EmptyExpression).and_then(|c| {
        match c.into() {
            '!' => nonbinary(i, nested).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb))),
            '?' => nonbinary(i, nested).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb))),
            '(' => start(i, true).map(|nb| Nonbinary::Parenthesized(Box::new(nb))),
            ')' => Triage::Error(Error::EmptyExpression),
            name => Name::from_char(name).map(Nonbinary::Value),
        }
        .and_then(|nb| {
            let mut acc = SyntaxAware::from(nb);
            let mut warn = None;
            loop {
                match next_op(i, nested) {
                    Triage::Okay(None) => return Triage::Okay(acc),
                    Triage::Warning(None, w) => {
                        return Triage::Warning(acc, warn.unwrap_or(w));
                    }
                    Triage::Okay(Some((op, arg))) => acc = shunt(acc, op, arg),
                    Triage::Warning(Some((op, arg)), w) => {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = warn.insert(w);
                        acc = shunt(acc, op, arg);
                    }
                    Triage::Error(e) => return Triage::Error(e),
                }
            }
        })
    })
}

/// Parse everything but infix operators.
#[inline]
#[allow(clippy::only_used_in_recursion)]
fn nonbinary<I: Iterator>(i: &mut I, nested: bool) -> Triage<Nonbinary, Warning, Error>
where
    I::Item: Into<char>,
{
    loop {
        return if let Some(c) = i.next() {
            match c.into() {
                '!' => nonbinary(i, nested).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb))),
                '?' => nonbinary(i, nested).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb))),
                '(' => start(i, true)
                    .and_then(|flat| Triage::Okay(Nonbinary::Parenthesized(Box::new(flat)))),
                ')' => Triage::Error(Error::InfixWithoutRightOperand),
                ' ' => continue,
                name => Name::from_char(name).map(Nonbinary::Value),
            }
        } else {
            Triage::Error(Error::End)
        };
    }
}

/// Parse the next operation and non-binary term without operator precedence.
#[inline]
fn next_op<I: Iterator>(
    i: &mut I,
    nested: bool,
) -> Triage<Option<(Infix, Nonbinary)>, Warning, Error>
where
    I::Item: Into<char>,
{
    loop {
        return if let Some(c) = i.next() {
            match c.into() {
                ' ' => continue,
                '*' => nonbinary(i, nested).map(|nb| Some((Infix::Times, nb))),
                '+' => nonbinary(i, nested).map(|nb| Some((Infix::Plus, nb))),
                '&' => nonbinary(i, nested).map(|nb| Some((Infix::With, nb))),
                PAR => nonbinary(i, nested).map(|nb| Some((Infix::Par, nb))),
                ')' => {
                    if nested {
                        Triage::Okay(None)
                    } else {
                        Triage::Error(Error::MissingLeftParen)
                    }
                }
                unrecognized => Triage::Error(Error::UnrecognizedInfixOperator(unrecognized)),
            }
        } else if nested {
            Triage::Error(Error::MissingRightParen)
        } else {
            Triage::Okay(None)
        };
    }
}

/// Resolve operator precedence for a single operation.
#[inline]
fn shunt(lhs: SyntaxAware, op: Infix, rhs: Nonbinary) -> SyntaxAware {
    match lhs {
        // No problem unless the left-hand side is a binary operation
        SyntaxAware::Value(_) | SyntaxAware::Unary(_, _) | SyntaxAware::Parenthesized(_) => {
            SyntaxAware::Binary(Box::new(lhs), op, Box::new(rhs.into()))
        }
        // Operator precedence time
        SyntaxAware::Binary(llhs, lop, lrhs) => match lop.cmp(&op) {
            core::cmp::Ordering::Less => SyntaxAware::Binary(
                Box::new(SyntaxAware::Binary(llhs, lop, lrhs)),
                op,
                Box::new(rhs.into()),
            ),
            core::cmp::Ordering::Equal => match op.associativity() {
                ast::Associativity::Left => SyntaxAware::Binary(
                    Box::new(SyntaxAware::Binary(llhs, lop, lrhs)),
                    op,
                    Box::new(rhs.into()),
                ),
                ast::Associativity::Right => SyntaxAware::Binary(
                    llhs,
                    lop,
                    Box::new(SyntaxAware::Binary(lrhs, op, Box::new(rhs.into()))),
                ),
            },
            core::cmp::Ordering::Greater => SyntaxAware::Binary(
                llhs,
                lop,
                Box::new(SyntaxAware::Binary(lrhs, op, Box::new(rhs.into()))),
            ),
        },
    }
}
