/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Parsing linear-logic expressions.

use crate::{
    ast::{self, Infix, Name, Nonbinary, Prefix, SyntaxAware, Tree, PAR},
    OptionWarn, Triage,
};

/// Integer type to hold parenthesis depth.
type Depth = u8;

/// Any non-fatal warning that could occur parsing a linear-logic expression.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Warning {
    /// Leading separated: e.g. ` A`.
    LeadingSpace,
    /// Trailing separated: e.g. `A `.
    TrailingSpace,
    /// Missing a space between operators: e.g. `A&B`.
    MissingInfixSpace(char),
    /// Too many spaces between operators: e.g. `A  &  B` or even `! A`.
    UnnecessarySpace,
    /// Unnecessary parentheses: e.g. `(A)`.
    UnnecessaryParentheses,
}

/// Any fatal error that could occur parsing a linear-logic expression.
#[non_exhaustive]
#[cfg_attr(test, allow(variant_size_differences))]
#[cfg_attr(not(test), derive(Copy))]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Error {
    /// Empty expression: e.g. "" or "()".
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
    /// Maximum parenthesis depth exceeded: e.g. `((((((((...))))))))`.
    MaxDepth,
    #[cfg(test)]
    /// State not separated when calling `start`. Internal use only.
    StateNotSeparated(Vec<char>),
}

/// State while parsing.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct State {
    /// Whether we're inside parentheses.
    pub(crate) depth: Depth,
    /// Whether the last character was a space (unstable: . . . or a unary operator).
    pub(crate) separated: bool,
}

#[allow(clippy::derivable_impls)]
impl Default for State {
    fn default() -> Self {
        State {
            depth: 0,
            separated: true,
        }
    }
}

impl State {
    /// Warn if the preceding character was not a space.
    pub(crate) const fn check_sep(self, c: char) -> Triage<(), Warning, Error> {
        if self.separated {
            Triage::Okay(())
        } else {
            Triage::Warn((), Warning::MissingInfixSpace(c))
        }
    }
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
#[inline(always)]
pub fn parse<I: IntoIterator>(i: I) -> Triage<Tree, Warning, Error>
where
    I::Item: Into<char>,
{
    start(&mut i.into_iter(), &mut State::default())
}

/// Parse a linear-logic expression from the beginning.
#[inline]
fn start<I: Iterator>(i: &mut I, state: &mut State) -> Triage<Tree, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = None;
    loop {
        #[cfg(test)]
        {
            if !state.separated {
                return Triage::Error(Error::StateNotSeparated(i.map(Into::into).collect()));
            }
        }
        return warn.warn_and_then(match i.next() {
            None => Triage::Error(Error::EmptyExpression),
            Some(c) => match c.into() {
                '!' => state.check_sep('!').and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb)))
                }),
                '?' => state.check_sep('?').and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb)))
                }),
                '(' => state.check_sep('(').and_then(|_| {
                    if let Some(deeper) = state.depth.checked_add(1) {
                        state.depth = deeper;
                    } else {
                        return Triage::Error(Error::MaxDepth);
                    }
                    start(i, state).map(|nb| Nonbinary::Parenthesized(Box::new(nb)))
                }),
                ')' => Triage::Error(Error::EmptyExpression),
                ' ' => {
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = warn.get_or_insert(Warning::LeadingSpace);
                    continue;
                }
                name => state.check_sep(name).and_then(|_| {
                    state.separated = false;
                    Name::from_char(name).map(Nonbinary::Value)
                }),
            }
            .and_then(|nb| tail(i, state, nb).map(Into::into)),
        });
    }
}

/// Parse all non-first pairs of operators and immediate right-hand non-binary terms.
#[inline]
fn tail<I: Iterator>(
    i: &mut I,
    state: &mut State,
    nb: Nonbinary,
) -> Triage<SyntaxAware, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut acc = SyntaxAware::from(nb);
    #[allow(clippy::shadow_unrelated)]
    let mut warn = None;
    loop {
        match next_op(i, state) {
            Triage::Okay(None) => return warn.warn_or(acc),
            Triage::Warn(None, w) => {
                return Triage::Warn(acc, warn.unwrap_or(w));
            }
            Triage::Okay(Some((op, arg))) => acc = fix_precedence(acc, op, arg),
            Triage::Warn(Some((op, arg)), w) => {
                #[allow(clippy::let_underscore_must_use)]
                let _ = warn.get_or_insert(w);
                acc = fix_precedence(acc, op, arg);
            }
            Triage::Error(e) => return Triage::Error(e),
        }
    }
}

/// Parse everything but infix operators.
#[inline]
#[allow(clippy::only_used_in_recursion)]
fn nonbinary<I: Iterator>(i: &mut I, state: &mut State) -> Triage<Nonbinary, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = None;
    loop {
        return warn.warn_and_then(if let Some(c) = i.next() {
            match c.into() {
                '!' => state.check_sep('!').and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb)))
                }),
                '?' => state.check_sep('?').and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb)))
                }),
                '(' => state.check_sep('(').and_then(|_| {
                    if let Some(deeper) = state.depth.checked_add(1) {
                        state.depth = deeper;
                    } else {
                        return Triage::Error(Error::MaxDepth);
                    }
                    start(i, state).map(|flat| Nonbinary::Parenthesized(Box::new(flat)))
                }),
                ')' => Triage::Error(Error::InfixWithoutRightOperand),
                ' ' => {
                    if state.separated {
                        let _ = warn.get_or_insert(Warning::UnnecessarySpace);
                    } else {
                        state.separated = true;
                    }
                    continue;
                }
                name => state.check_sep(name).and_then(|_| {
                    state.separated = false;
                    Name::from_char(name).map(Nonbinary::Value)
                }),
            }
        } else {
            Triage::Error(Error::End)
        });
    }
}

/// Parse the next operation and non-binary term without operator precedence.
#[inline]
fn next_op<I: Iterator>(
    i: &mut I,
    state: &mut State,
) -> Triage<Option<(Infix, Nonbinary)>, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = None;
    loop {
        return warn.warn_and_then(if let Some(c) = i.next() {
            match c.into() {
                ' ' => {
                    if state.separated {
                        let _ = warn.get_or_insert(Warning::UnnecessarySpace);
                    } else {
                        state.separated = true;
                    }
                    continue;
                }
                '*' => state.check_sep('*').and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::Times, nb)))
                }),
                '+' => state.check_sep('+').and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::Plus, nb)))
                }),
                '&' => state.check_sep('&').and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::With, nb)))
                }),
                PAR => state.check_sep(PAR).and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::Par, nb)))
                }),
                ')' => {
                    if let Some(deeper) = state.depth.checked_sub(1) {
                        state.depth = deeper;
                        if state.separated {
                            state.separated = false;
                            Triage::Warn(None, Warning::TrailingSpace)
                        } else {
                            Triage::Okay(None)
                        }
                    } else {
                        Triage::Error(Error::MissingLeftParen)
                    }
                }
                unrecognized => Triage::Error(Error::UnrecognizedInfixOperator(unrecognized)),
            }
        } else if state.depth != 0 {
            Triage::Error(Error::MissingRightParen)
        } else if state.separated {
            Triage::Warn(None, Warning::TrailingSpace)
        } else {
            Triage::Okay(None)
        });
    }
}

/// Resolve operator precedence for a single operation.
#[inline]
fn fix_precedence(lhs: SyntaxAware, op: Infix, rhs: Nonbinary) -> SyntaxAware {
    match lhs {
        // No problem unless the left-hand side is a binary operation
        SyntaxAware::Value(_) | SyntaxAware::Unary(_, _) | SyntaxAware::Parenthesized(_) => {
            SyntaxAware::Binary(Box::new(lhs.into()), op, Box::new(rhs.into()))
        }
        // Operator precedence time
        SyntaxAware::Binary(llhs, lop, lrhs) => match lop.cmp(&op) {
            core::cmp::Ordering::Less => SyntaxAware::Binary(
                Box::new(Tree::Binary(llhs, lop, Box::new((*lrhs).into()))),
                op,
                Box::new(rhs.into()),
            ),
            core::cmp::Ordering::Equal => match op.associativity() {
                ast::Associativity::Left => SyntaxAware::Binary(
                    Box::new(Tree::Binary(llhs, lop, Box::new((*lrhs).into()))),
                    op,
                    Box::new(rhs.into()),
                ),
                ast::Associativity::Right => SyntaxAware::Binary(
                    llhs,
                    lop,
                    Box::new(SyntaxAware::Binary(
                        Box::new((*lrhs).into()),
                        op,
                        Box::new(rhs.into()),
                    )),
                ),
            },
            core::cmp::Ordering::Greater => SyntaxAware::Binary(
                llhs,
                lop,
                Box::new(SyntaxAware::Binary(
                    Box::new((*lrhs).into()),
                    op,
                    Box::new(rhs.into()),
                )),
            ),
        },
    }
}
