/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![allow(
    clippy::missing_const_for_fn,
    clippy::print_stdout,
    clippy::unwrap_used,
    clippy::use_debug,
    unused_imports
)]

use crate::{
    ast::{self, Name},
    parse, Triage,
};

mod unit {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(
            parse(core::iter::empty::<char>()),
            Triage::Error(parse::Error::EmptyExpression),
        );
    }

    #[test]
    fn empty_parens() {
        assert_eq!(
            parse("()".chars()),
            Triage::Error(parse::Error::EmptyExpression),
        );
    }

    #[test]
    fn name() {
        assert_eq!(
            parse("A".chars()),
            Triage::Okay(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
        );
    }

    #[test]
    fn bang() {
        assert_eq!(
            parse("!A".chars()),
            Triage::Okay(ast::Tree::Unary(
                ast::Prefix::Bang,
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            )),
        );
    }

    #[test]
    fn times() {
        assert_eq!(
            parse("A * A".chars()),
            Triage::Okay(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            )),
        );
    }

    #[test]
    fn unnecessary_space() {
        assert_eq!(
            parse("! A".chars()),
            Triage::Warn(
                ast::Tree::Unary(
                    ast::Prefix::Bang,
                    Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap()))
                ),
                parse::Warning::UnnecessarySpace,
            ),
        );
    }

    #[test]
    fn unnecessary_parens() {
        assert_eq!(
            parse("A + (B * C)".chars()),
            Triage::Warn(
                ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                    ast::Infix::Plus,
                    Box::new(ast::Tree::Binary(
                        Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
                        ast::Infix::Times,
                        Box::new(ast::Tree::Value(Name::from_char('C').strict().unwrap())),
                    )),
                ),
                parse::Warning::UnnecessaryParens,
            ),
        );
    }

    #[test]
    #[allow(unused_results)]
    fn double_parens() {
        assert_eq!(
            parse("((A + B)) * C".chars()),
            Triage::Warn(
                ast::Tree::Binary(
                    Box::new(ast::Tree::Binary(
                        Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                        ast::Infix::Plus,
                        Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
                    )),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('C').strict().unwrap())),
                ),
                parse::Warning::UnnecessaryParens,
            ),
        );
    }
}

#[cfg(feature = "quickcheck")]
mod prop {
    use super::*;

    quickcheck::quickcheck! {
        fn roundtrip_expr_bytes(tree: ast::Tree) -> bool {
            let printed = format!("{tree}");
            let parsed = parse(printed.chars());
            parsed == Triage::Okay(tree)
        }

        fn roundtrip_bytes_expr(bytes: Vec<u8>) -> quickcheck::TestResult {
            let Triage::Okay(parsed) = parse(bytes.iter().copied()) else { return quickcheck::TestResult::discard(); };
            let printed = format!("{parsed}");
            quickcheck::TestResult::from_bool(printed.chars().eq(bytes.into_iter().map(Into::into)))
        }

        fn roundtrip_chars_expr(chars: String) -> quickcheck::TestResult {
            let Triage::Okay(parsed) = parse(chars.chars()) else { return quickcheck::TestResult::discard(); };
            let printed = format!("{parsed}");
            quickcheck::TestResult::from_bool(printed == chars)
        }
    }
}

mod reduced {
    use super::*;

    #[test]
    fn roundtrip_expr_bytes_1() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "A * B");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_2() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Unary(
                ast::Prefix::Bang,
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            )),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!A * B");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_3() {
        let tree = ast::Tree::Unary(
            ast::Prefix::Bang,
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!(A * B)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_4() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                ast::Infix::Plus,
                Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
            )),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('C').strict().unwrap())),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "(A + B) * C");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_5() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            ast::Infix::Times,
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('C').strict().unwrap())),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "A * (B * C)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_6() {
        let tree = ast::Tree::Unary(
            ast::Prefix::Bang,
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('C').strict().unwrap())),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!(A * (B * C))");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_1() {
        assert_eq!(
            parse("A ".chars()),
            Triage::Warn(
                ast::Tree::Value(Name::from_char('A').strict().unwrap()),
                parse::Warning::TrailingSpace,
            ),
        );
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_2() {
        assert_eq!(
            parse("A* B".chars()),
            Triage::Warn(
                ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
                ),
                parse::Warning::MissingInfixSpace,
            ),
        );
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_3() {
        assert_eq!(
            parse("A *B".chars()),
            Triage::Warn(
                ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('B').strict().unwrap())),
                ),
                parse::Warning::MissingInfixSpace,
            ),
        );
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_4() {
        assert_eq!(
            parse("(A)".chars()),
            Triage::Warn(
                ast::Tree::Value(Name::from_char('A').strict().unwrap()),
                parse::Warning::UnnecessaryParens,
            ),
        );
    }
}

mod systematic {
    use super::*;

    #[repr(u8)]
    enum Character {
        A,
        Unary,
        Binary,
        LParen,
        RParen,
        Space,
    }

    impl From<Character> for char {
        #[inline(always)]
        fn from(value: Character) -> Self {
            match value {
                Character::A => 'A',
                Character::Unary => '!',
                Character::Binary => '*',
                Character::LParen => '(',
                Character::RParen => ')',
                Character::Space => ' ',
            }
        }
    }

    impl From<u8> for Character {
        #[inline(always)]
        fn from(value: u8) -> Self {
            #[allow(clippy::unreachable)]
            match value {
                0 => Character::A,
                1 => Character::Unary,
                2 => Character::Binary,
                3 => Character::LParen,
                4 => Character::RParen,
                5 => Character::Space,
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn exhaustive_and_valid_iff_both_unique_and_shortest() {
        const MIN_LEN: usize = 1;
        const MAX_LEN: usize = 11; // TODO: way up
        let mut v = vec![];
        while v.len() < MIN_LEN {
            v.push(0);
        }
        let mut seen = std::collections::HashSet::new();
        // Depth 2 exhausted with length 11
        let mut exhaustive = ast::Tree::exhaustive_up_to_depth(2); // TODO: way up
        {
            let len = exhaustive.len();
            exhaustive.dedup();
            assert_eq!(len, exhaustive.len()); // no-op
        }
        exhaustive.sort();
        let mut exhausted: Vec<_> = exhaustive.iter().map(|_| false).collect();
        assert_eq!(exhaustive.len(), exhausted.len());
        'check: loop {
            let chars = v.iter().map(|b| char::from(Character::from(*b)));
            #[allow(clippy::wildcard_enum_match_arm)]
            match parse(chars.clone()) {
                Triage::Okay(parsed) => {
                    #[allow(unsafe_code)]
                    if let Ok(i) = exhaustive.binary_search(&parsed) {
                        // SAFETY:
                        // Same length, and the searched vector is sorted.
                        *unsafe { exhausted.get_unchecked_mut(i) } = true;
                    }
                    assert!(
                        seen.insert(parsed),
                        "Fully valid string \"{}\" parsed \
                        as an expression we've already seen",
                        chars.collect::<String>(),
                    );
                }
                Triage::Warn(parsed, parse::Warning::UnnecessaryParens) => {
                    // must not be the shortest way to write it,
                    // or else the parens would have been necessary
                    assert!(
                        seen.contains(&parsed),
                        "Parentheses flagged as unnecessary in \"{}\", \
                        but there's no shorter way to write `{parsed:?}`",
                        v.iter()
                            .map(|b| char::from(Character::from(*b)))
                            .collect::<String>(),
                    );
                }
                _ => {}
            }
            let mut i = v.len().checked_sub(1).unwrap();
            'carry: loop {
                #[allow(clippy::get_unwrap, unsafe_code)]
                let c = v.get_mut(i).unwrap();
                if *c < 5 {
                    *c += 1;
                    continue 'check;
                }
                *c = 0;
                if let Some(decr) = i.checked_sub(1) {
                    i = decr;
                    continue 'carry;
                }
                if v.len() < MAX_LEN {
                    v.push(0);
                    continue 'check;
                }
                for j in 0..exhaustive.len() {
                    #[allow(unsafe_code)]
                    // SAFETY: Range above. Also asserted to be the same length earlier.
                    unsafe {
                        assert!(
                            exhausted.get_unchecked(j),
                            "Couldn't produce \"{}\" i.e. `{0:?}`",
                            exhaustive.get_unchecked(j)
                        );
                    }
                }
                return;
            }
        }
    }
}
