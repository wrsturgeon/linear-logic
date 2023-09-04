/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Parsing linear-logic expressions.

// TODO: abolish `usize::MAX`

use crate::{
    ast::{tree::SyntaxAware, Infix, Name, Nonbinary, Prefix, Tree, PAR},
    Spanned, Triage,
};
use core::iter::Enumerate;

/// Any non-fatal warning that could occur parsing a linear-logic expression.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Warning {
    /// Unnecessary parentheses: e.g. `(A)`.
    UnnecessaryParens,
    /// Too many spaces between operators: e.g. `A  &  B` or even `! A`.
    UnnecessarySpace,
    /// Trailing separated: e.g. `A `.
    TrailingSpace,
    /// Leading separated: e.g. ` A`.
    LeadingSpace,
    /// Missing a space between operators: e.g. `A&B`.
    MissingInfixSpace,
}

impl core::fmt::Display for Warning {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[TODO: `Warning` formatting]")
    }
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
    #[cfg(test)]
    /// State not separated when calling `start`. Internal use only.
    StateNotSeparated(Vec<char>),
    /// Maximum parentheses depth exceeded: e.g. `((((((((((...))))))))))`
    MaxDepth,
}

impl core::fmt::Display for Error {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[TODO: `Error` formatting]")
    }
}

/// Type to hold the number of parentheses around an expression.
type Depth = u8;

/// State while parsing.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct State {
    /// Whether the last character was a space (unstable: . . . or a unary operator).
    pub(crate) separated: bool,
    /// Layers of parenthesization.
    pub(crate) depth: Depth,
}

#[allow(clippy::derivable_impls)]
impl Default for State {
    fn default() -> Self {
        State {
            separated: true,
            depth: 0,
        }
    }
}

impl State {
    /// Warn if the preceding character was not a space.
    pub(crate) const fn check_sep(&self, index: usize) -> Triage<(), Warning, Error> {
        if self.separated {
            Triage::Okay(())
        } else {
            Triage::Warn(
                (),
                Spanned {
                    msg: Warning::MissingInfixSpace,
                    index,
                },
            )
        }
    }
}

/// Parse a linear-logic expression from an iterator over anything that can be converted to characters.
#[inline(always)]
pub fn parse<I: IntoIterator>(i: I) -> Triage<Tree, Warning, Error>
where
    I::Item: Into<char>,
{
    start(&mut i.into_iter().enumerate(), &mut State::default())
        .and_then(|sa| sa.into_tree(None, None))
}

/// Parse a linear-logic expression from the beginning.
#[inline]
fn merged<I: Iterator>(
    iter: &mut Enumerate<I>,
    state: &mut State,
    starting: bool,
) -> Triage<Nonbinary, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = Triage::Okay(());
    loop {
        let ok = match iter.next() {
            None => Triage::Error(Spanned {
                msg: if starting {
                    Error::EmptyExpression
                } else {
                    Error::End
                },
                index: usize::MAX,
            }),
            Some((index, c)) => match c.into() {
                '!' => state.check_sep(index).and_then(|_| {
                    nonbinary(iter, state).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb)))
                }),
                '?' => state.check_sep(index).and_then(|_| {
                    nonbinary(iter, state).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb)))
                }),
                '(' => state.check_sep(index).and_then(|_| {
                    if let Some(incr) = state.depth.checked_add(1) {
                        state.depth = incr;
                    } else {
                        return Triage::Error(Spanned {
                            msg: Error::MaxDepth,
                            index,
                        });
                    }
                    start(iter, state).and_then(|sa| match sa {
                        SyntaxAware::Binary(lhs, op, rhs) => {
                            Triage::Okay(Nonbinary::Parenthesized(lhs, op, rhs, index))
                        }
                        SyntaxAware::Value(name) => Triage::Warn(
                            Nonbinary::Value(name),
                            Spanned {
                                msg: Warning::UnnecessaryParens,
                                index,
                            },
                        ),
                        SyntaxAware::Unary(op, arg) => Triage::Warn(
                            Nonbinary::Unary(op, arg),
                            Spanned {
                                msg: Warning::UnnecessaryParens,
                                index,
                            },
                        ),
                        SyntaxAware::Parenthesized(lhs, op, rhs, i) => Triage::Warn(
                            Nonbinary::Parenthesized(lhs, op, rhs, i),
                            Spanned {
                                msg: Warning::UnnecessaryParens,
                                index: i,
                            },
                        ),
                    })
                }),
                ')' => Triage::Error(Spanned {
                    msg: if starting {
                        if state.depth == 0 {
                            Error::MissingLeftParen
                        } else {
                            Error::EmptyExpression
                        }
                    } else {
                        Error::InfixWithoutRightOperand
                    },
                    index,
                }),
                ' ' => {
                    if starting {
                        warn = warn.max(Triage::Warn(
                            (),
                            Spanned {
                                msg: Warning::LeadingSpace,
                                index,
                            },
                        ));
                    } else if state.separated {
                        warn = warn.max(Triage::Warn(
                            (),
                            Spanned {
                                msg: Warning::UnnecessarySpace,
                                index,
                            },
                        ));
                    } else {
                        state.separated = true;
                    }
                    continue;
                }
                name => state.check_sep(index).and_then(|_| {
                    state.separated = false;
                    Name::from_char(name, index).map(Nonbinary::Value)
                }),
            },
        };
        return warn.and_then(|()| ok);
    }
}

/// Parse a linear-logic expression from the beginning.
#[inline(always)]
fn start<I: Iterator>(
    iter: &mut Enumerate<I>,
    state: &mut State,
) -> Triage<SyntaxAware, Warning, Error>
where
    I::Item: Into<char>,
{
    merged(iter, state, true).and_then(|nb| tail(iter, state, nb))
}

/// Parse all non-first pairs of operators and immediate right-hand non-binary terms.
#[inline]
fn tail<I: Iterator>(
    iter: &mut Enumerate<I>,
    state: &mut State,
    nb: Nonbinary,
) -> Triage<SyntaxAware, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut acc = SyntaxAware::from(nb);
    #[allow(clippy::shadow_unrelated)]
    let mut warn = Triage::Okay(());
    loop {
        match next_op(iter, state) {
            Triage::Okay(None) => return warn.map(|()| acc),
            Triage::Warn(None, w) => {
                return warn.max(Triage::Warn((), w)).map(|()| acc);
            }
            Triage::Okay(Some((op, arg))) => {
                acc = fix_precedence(acc, op, arg);
            }
            Triage::Warn(Some((op, arg)), w) => {
                warn = warn.max(Triage::Warn((), w));
                acc = fix_precedence(acc, op, arg);
            }
            Triage::Error(e) => return Triage::Error(e),
        }
    }
}

/// Parse everything but infix operators.
#[inline(always)]
#[allow(clippy::only_used_in_recursion)]
fn nonbinary<I: Iterator>(
    iter: &mut Enumerate<I>,
    state: &mut State,
) -> Triage<Nonbinary, Warning, Error>
where
    I::Item: Into<char>,
{
    merged(iter, state, false)
}

/// Parse the next operation and non-binary term without operator precedence.
#[inline]
fn next_op<I: Iterator>(
    iter: &mut Enumerate<I>,
    state: &mut State,
) -> Triage<Option<(Infix, Nonbinary)>, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = Triage::Okay(());
    loop {
        let ok = if let Some((index, c)) = iter.next() {
            match c.into() {
                ' ' => {
                    if state.separated {
                        warn = warn.max(Triage::Warn(
                            (),
                            Spanned {
                                msg: Warning::UnnecessarySpace,
                                index,
                            },
                        ));
                    } else {
                        state.separated = true;
                    }
                    continue;
                }
                '*' => state.check_sep(index).and_then(|_| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::Times, nb)))
                }),
                '+' => state.check_sep(index).and_then(|_| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::Plus, nb)))
                }),
                '&' => state.check_sep(index).and_then(|_| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::With, nb)))
                }),
                PAR => state.check_sep(index).and_then(|_| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::Par, nb)))
                }),
                ')' => {
                    if let Some(decr) = state.depth.checked_sub(1) {
                        state.depth = decr;
                        if state.separated {
                            state.separated = false;
                            Triage::Warn(
                                None,
                                Spanned {
                                    msg: Warning::TrailingSpace,
                                    index,
                                },
                            )
                        } else {
                            Triage::Okay(None)
                        }
                    } else {
                        Triage::Error(Spanned {
                            msg: Error::MissingLeftParen,
                            index,
                        })
                    }
                }
                unrecognized => Triage::Error(Spanned {
                    msg: Error::UnrecognizedInfixOperator(unrecognized),
                    index,
                }),
            }
        } else if state.depth != 0 {
            Triage::Error(Spanned {
                msg: Error::MissingRightParen,
                index: usize::MAX,
            })
        } else if state.separated {
            Triage::Warn(
                None,
                Spanned {
                    msg: Warning::TrailingSpace,
                    index: usize::MAX,
                },
            )
        } else {
            Triage::Okay(None)
        };
        return warn.and_then(|()| ok);
    }
}

/// Resolve operator precedence for a single operation.
#[inline]
#[allow(clippy::similar_names)]
fn fix_precedence(lhs: SyntaxAware, op: Infix, rhs: Nonbinary) -> SyntaxAware {
    match lhs {
        // No problem unless the left-hand side is a binary operation
        SyntaxAware::Value(_)
        | SyntaxAware::Unary(_, _)
        | SyntaxAware::Parenthesized(_, _, _, _) => {
            SyntaxAware::Binary(Box::new(lhs), op, Box::new(rhs.into()))
        }
        // Operator precedence time
        SyntaxAware::Binary(llhs, lop, lrhs) => {
            if lop.weaker_to_the_left_of(op) {
                SyntaxAware::Binary(llhs, lop, Box::new(fix_precedence(*lrhs, op, rhs)))
            } else {
                SyntaxAware::Binary(
                    Box::new(SyntaxAware::Binary(llhs, lop, lrhs)),
                    op,
                    Box::new(rhs.into()),
                )
            }
        }
    }
}
