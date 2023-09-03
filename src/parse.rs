/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Parsing linear-logic expressions.

use crate::{
    ast::{self, tree::SyntaxAware, Infix, Name, Nonbinary, Prefix, Tree, PAR},
    Triage,
};

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
    pub(crate) const fn check_sep(&self) -> Triage<(), Warning, Error> {
        if self.separated {
            Triage::Okay(())
        } else {
            Triage::Warn((), Warning::MissingInfixSpace)
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
//                     "Unrecognized prefix operator '{}'. Rest of the input: `{v:?}`",
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
    start(&mut i.into_iter(), &mut State::default()).and_then(|sa| sa.into_tree(None, None))
}

/// Parse a linear-logic expression from the beginning.
#[inline]
fn start<I: Iterator>(i: &mut I, state: &mut State) -> Triage<SyntaxAware, Warning, Error>
where
    I::Item: Into<char>,
{
    let mut warn = Triage::Okay(());
    loop {
        #[cfg(test)]
        {
            if !state.separated {
                return Triage::Error(Error::StateNotSeparated(i.map(Into::into).collect()));
            }
        }
        let ok = match i.next() {
            None => Triage::Error(Error::EmptyExpression),
            Some(c) => match c.into() {
                '!' => state.check_sep().and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb)))
                }),
                '?' => state.check_sep().and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb)))
                }),
                '(' => state.check_sep().and_then(|_| {
                    if let Some(incr) = state.depth.checked_add(1) {
                        state.depth = incr;
                    } else {
                        return Triage::Error(Error::MaxDepth);
                    }
                    start(i, state).and_then(|sa| match sa {
                        SyntaxAware::Binary(lhs, op, rhs) => {
                            Triage::Okay(Nonbinary::Parenthesized(lhs, op, rhs))
                        }
                        SyntaxAware::Value(name) => {
                            Triage::Warn(Nonbinary::Value(name), Warning::UnnecessaryParens)
                        }
                        SyntaxAware::Unary(op, arg) => {
                            Triage::Warn(Nonbinary::Unary(op, arg), Warning::UnnecessaryParens)
                        }
                        SyntaxAware::Parenthesized(lhs, op, rhs) => Triage::Warn(
                            Nonbinary::Parenthesized(lhs, op, rhs),
                            Warning::UnnecessaryParens,
                        ),
                    })
                }),
                ')' => Triage::Error(Error::EmptyExpression),
                ' ' => {
                    warn = warn.max(Triage::Warn((), Warning::LeadingSpace));
                    continue;
                }
                name => state.check_sep().and_then(|_| {
                    state.separated = false;
                    Name::from_char(name).map(Nonbinary::Value)
                }),
            }
            .and_then(|nb| tail(i, state, nb)),
        };
        return warn.and_then(|()| ok);
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
    let mut warn = Triage::Okay(());
    loop {
        match next_op(i, state) {
            Triage::Okay(None) => return warn.map(|()| acc),
            Triage::Warn(None, w) => {
                return warn.max(Triage::Warn((), w)).map(|()| acc);
            }
            Triage::Okay(Some((op, arg))) => {
                let (prec, _, maybe_warn) = fix_precedence(None, acc, op, arg, None);
                acc = prec;
                if let Some(w) = maybe_warn {
                    warn = warn.max(Triage::Warn((), w));
                }
            }
            Triage::Warn(Some((op, arg)), w) => {
                warn = warn.max(Triage::Warn((), w));
                acc = fix_precedence(None, acc, op, arg, None).0;
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
    let mut warn = Triage::Okay(());
    loop {
        let ok = if let Some(c) = i.next() {
            match c.into() {
                '!' => state.check_sep().and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Bang, Box::new(nb)))
                }),
                '?' => state.check_sep().and_then(|_| {
                    nonbinary(i, state).map(|nb| Nonbinary::Unary(Prefix::Quest, Box::new(nb)))
                }),
                '(' => state.check_sep().and_then(|_| {
                    if let Some(incr) = state.depth.checked_add(1) {
                        state.depth = incr;
                    } else {
                        return Triage::Error(Error::MaxDepth);
                    }
                    start(i, state).and_then(|sa| match sa {
                        SyntaxAware::Binary(lhs, op, rhs) => {
                            Triage::Okay(Nonbinary::Parenthesized(lhs, op, rhs))
                        }
                        SyntaxAware::Value(name) => {
                            Triage::Warn(Nonbinary::Value(name), Warning::UnnecessaryParens)
                        }
                        SyntaxAware::Unary(op, arg) => {
                            Triage::Warn(Nonbinary::Unary(op, arg), Warning::UnnecessaryParens)
                        }
                        SyntaxAware::Parenthesized(lhs, op, arg) => Triage::Warn(
                            Nonbinary::Parenthesized(lhs, op, arg),
                            Warning::UnnecessaryParens,
                        ),
                    })
                }),
                ')' => Triage::Error(Error::InfixWithoutRightOperand),
                ' ' => {
                    if state.separated {
                        warn = warn.max(Triage::Warn((), Warning::UnnecessarySpace));
                    } else {
                        state.separated = true;
                    }
                    continue;
                }
                name => state.check_sep().and_then(|_| {
                    state.separated = false;
                    Name::from_char(name).map(Nonbinary::Value)
                }),
            }
        } else {
            Triage::Error(Error::End)
        };
        return warn.and_then(|()| ok);
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
    let mut warn = Triage::Okay(());
    loop {
        let ok = if let Some(c) = i.next() {
            match c.into() {
                ' ' => {
                    if state.separated {
                        warn = warn.max(Triage::Warn((), Warning::UnnecessarySpace));
                    } else {
                        state.separated = true;
                    }
                    continue;
                }
                '*' => state.check_sep().and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::Times, nb)))
                }),
                '+' => state.check_sep().and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::Plus, nb)))
                }),
                '&' => state.check_sep().and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::With, nb)))
                }),
                PAR => state.check_sep().and_then(|_| {
                    state.separated = false;
                    nonbinary(i, state).map(|nb| Some((Infix::Par, nb)))
                }),
                ')' => {
                    if let Some(decr) = state.depth.checked_sub(1) {
                        state.depth = decr;
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
        };
        return warn.and_then(|()| ok);
    }
}

/// Resolve operator precedence for a single operation.
#[inline]
#[allow(clippy::similar_names)]
fn fix_precedence(
    _lnbr: Option<Infix>,
    lhs: SyntaxAware,
    op: Infix,
    rhs: Nonbinary,
    _rnbr: Option<Infix>,
) -> (SyntaxAware, Option<Infix>, Option<Warning>) {
    match lhs {
        // No problem unless the left-hand side is a binary operation
        SyntaxAware::Value(_) | SyntaxAware::Unary(_, _) | SyntaxAware::Parenthesized(_, _, _) => (
            SyntaxAware::Binary(Box::new(lhs), op, Box::new(rhs.into())),
            None,
            None,
        ),
        // Operator precedence time
        SyntaxAware::Binary(llhs, lop, lrhs) => match lop.cmp(&op) {
            core::cmp::Ordering::Less => (
                SyntaxAware::Binary(
                    Box::new(SyntaxAware::Binary(llhs, lop, lrhs)),
                    op,
                    Box::new(rhs.into()),
                ),
                None,
                None,
            ),
            core::cmp::Ordering::Equal => match op.associativity() {
                ast::Associativity::Left => (
                    SyntaxAware::Binary(
                        Box::new(SyntaxAware::Binary(llhs, lop, lrhs)),
                        op,
                        Box::new(rhs.into()),
                    ),
                    None,
                    None,
                ),
                ast::Associativity::Right => (
                    SyntaxAware::Binary(
                        llhs,
                        lop,
                        Box::new(SyntaxAware::Binary(lrhs, op, Box::new(rhs.into()))),
                    ),
                    None,
                    None,
                ),
            },
            core::cmp::Ordering::Greater => (
                SyntaxAware::Binary(
                    llhs,
                    lop,
                    Box::new(SyntaxAware::Binary(lrhs, op, Box::new(rhs.into()))),
                ),
                None,
                None,
            ),
        },
    }
}
