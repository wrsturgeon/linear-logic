/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Parsing linear-logic expressions.

// TODO: abolish `usize::MAX`

use crate::{
    ast::{
        unsimplified::SyntaxAware, Atomic, Nonbinary, Unsimplified, UnsimplifiedInfix as Infix,
        UnsimplifiedPrefix as Prefix, PAR,
    },
    Spanned, Triage,
};
use core::iter::Enumerate;

/// Any non-fatal warning that could occur parsing a linear-logic expression.
#[repr(u8)]
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
        write!(
            f,
            "{}",
            match self {
                &Self::UnnecessaryParens => "unnecessary parentheses",
                &Self::UnnecessarySpace => "unnecessary space",
                &Self::TrailingSpace => "trailing space",
                &Self::LeadingSpace => "leading space",
                &Self::MissingInfixSpace => "missing a space around an infix operator",
            }
        )
    }
}

/// Any fatal error that could occur parsing a linear-logic expression.
#[repr(u8)]
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Error {
    /// Empty expression: e.g. "" or "()".
    EmptyExpression,
    /// Iterator ended but we needed more input.
    End,
    /// Unrecognized infix operator.
    UnrecognizedInfixOperator,
    /// Unrecognized prefix operator.
    UnrecognizedPrefix,
    /// Used an infix operator without an argument to its left.
    InfixWithoutLeftOperand,
    /// Used an infix operator without an argument to its right.
    InfixWithoutRightOperand,
    /// Missing a left (opening) parenthesis.
    MissingLeftParen,
    /// Missing a right (closing) parenthesis.
    MissingRightParen,
    /// Invalid name.
    InvalidName,
    /// Maximum parentheses depth exceeded: e.g. `((((((((((...))))))))))`
    MaxDepth,
}

impl core::fmt::Display for Error {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Self::EmptyExpression => "empty expression",
                &Self::End => "unexpected end of input",
                &Self::UnrecognizedInfixOperator => "unrecognized infix operator",
                &Self::UnrecognizedPrefix => "unrecognized prefix operator",
                &Self::InfixWithoutLeftOperand => "infix operator without a left operand",
                &Self::InfixWithoutRightOperand => "infix operator without a right operand",
                &Self::MissingLeftParen => "missing a left (opening) parenthesis",
                &Self::MissingRightParen => "missing a right (closing) parenthesis",
                &Self::InvalidName => "invalid character in a name",
                &Self::MaxDepth => "maximum parentheses exceeded",
            }
        )
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
pub fn parse<I: IntoIterator>(i: I) -> Triage<Unsimplified, Warning, Error>
where
    I::Item: Into<char>,
{
    start(&mut i.into_iter().enumerate(), &mut State::default())
        .and_then(|sa| sa.into_unsimplified(None, None))
}

/// Parse a linear-logic expression from the beginning.
#[inline]
#[allow(clippy::too_many_lines)]
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
                '!' => state.check_sep(index).and_then(|()| {
                    nonbinary(iter, state).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb)))
                }),
                '?' => state.check_sep(index).and_then(|()| {
                    nonbinary(iter, state).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb)))
                }),
                '~' => state.check_sep(index).and_then(|()| {
                    nonbinary(iter, state).map(|nb| Nonbinary::Unary(Prefix::Dual, Box::new(nb)))
                }),
                '(' => state.check_sep(index).and_then(|()| {
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
                        SyntaxAware::Atomic(atom) => Triage::Warn(
                            Nonbinary::Atomic(atom),
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
                '0' => state.check_sep(index).map(|()| {
                    state.separated = false;
                    Nonbinary::Atomic(Atomic::Zero)
                }),
                '1' => state.check_sep(index).map(|()| {
                    state.separated = false;
                    Nonbinary::Atomic(Atomic::One)
                }),
                '_' => state.check_sep(index).map(|()| {
                    state.separated = false;
                    Nonbinary::Atomic(Atomic::Bottom)
                }),
                'T' => state.check_sep(index).map(|()| {
                    state.separated = false;
                    Nonbinary::Atomic(Atomic::Top)
                }),
                other => state.check_sep(index).and_then(|()| {
                    state.separated = false;
                    Atomic::from_char(other, index).map(Nonbinary::Atomic)
                }),
            },
        };
        return warn.and_then(|()| ok);
    }
}

/// Postmortem of a premature ending that might be acceptable.
#[inline]
fn postmortem<T>(state: &mut State, value: T) -> Triage<T, Warning, Error> {
    if state.depth != 0 {
        Triage::Error(Spanned {
            msg: Error::MissingRightParen,
            index: usize::MAX,
        })
    } else if state.separated {
        Triage::Warn(
            value,
            Spanned {
                msg: Warning::TrailingSpace,
                index: usize::MAX,
            },
        )
    } else {
        Triage::Okay(value)
    }
}

/// After a potentially valid closing parenthesis.
#[inline]
fn close_paren<T>(state: &mut State, value: T, index: usize) -> Triage<T, Warning, Error> {
    if let Some(decr) = state.depth.checked_sub(1) {
        state.depth = decr;
        if state.separated {
            state.separated = false;
            Triage::Warn(
                value,
                Spanned {
                    msg: Warning::TrailingSpace,
                    index,
                },
            )
        } else {
            Triage::Okay(value)
        }
    } else {
        Triage::Error(Spanned {
            msg: Error::MissingLeftParen,
            index,
        })
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
        match next_op(iter, state, &mut acc) {
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
    acc: &mut SyntaxAware,
) -> Triage<Option<(Infix, Nonbinary)>, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = Triage::Okay(());
    loop {
        let ok = match iter.next() {
            None => postmortem(state, None),
            Some((index, c)) => match c.into() {
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
                '*' => state.check_sep(index).and_then(|()| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::Times, nb)))
                }),
                '+' => state.check_sep(index).and_then(|()| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::Plus, nb)))
                }),
                '&' => state.check_sep(index).and_then(|()| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::With, nb)))
                }),
                PAR => state.check_sep(index).and_then(|()| {
                    state.separated = false;
                    nonbinary(iter, state).map(|nb| Some((Infix::Par, nb)))
                }),
                '-' => state.check_sep(index).and_then(|()| {
                    state.separated = false;
                    match iter.next() {
                        None => Triage::Error(Spanned {
                            msg: Error::End,
                            index: usize::MAX,
                        }),
                        Some((idx, tip)) => {
                            if tip.into() == '>' {
                                nonbinary(iter, state).map(|nb| Some((Infix::Lollipop, nb)))
                            } else {
                                Triage::Error(Spanned {
                                    msg: Error::UnrecognizedInfixOperator,
                                    index: idx,
                                })
                            }
                        }
                    }
                }),
                ')' => close_paren(state, None, index),
                other => {
                    if let Some(name) = acc.reading_name() {
                        if name.push::<core::convert::Infallible>(other, index) == Triage::Okay(())
                        {
                            continue;
                        }
                    }
                    Triage::Error(Spanned {
                        msg: Error::UnrecognizedInfixOperator,
                        index,
                    })
                }
            },
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
        SyntaxAware::Atomic(_)
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
