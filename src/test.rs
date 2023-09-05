/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![allow(
    clippy::missing_const_for_fn,
    clippy::panic,
    clippy::print_stdout,
    clippy::unwrap_used,
    clippy::use_debug,
    unused_imports
)]

use crate::{
    ast::{self, Name},
    parse, Spanned, Triage,
};

mod unit {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(
            parse(core::iter::empty::<char>()),
            Triage::Error(Spanned {
                msg: parse::Error::EmptyExpression,
                index: usize::MAX,
            }),
        );
    }

    #[test]
    fn empty_parens() {
        assert_eq!(
            parse("()".chars()),
            Triage::Error(Spanned {
                msg: parse::Error::EmptyExpression,
                index: 1,
            }),
        );
    }

    #[test]
    fn just_rparen() {
        assert_eq!(
            parse(")".chars()),
            Triage::Error(Spanned {
                msg: parse::Error::MissingLeftParen,
                index: 0,
            })
        );
    }

    #[test]
    fn name() {
        assert_eq!(
            parse("A".chars()),
            Triage::Okay(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
        );
    }

    #[test]
    fn bang() {
        assert_eq!(
            parse("!A".chars()),
            Triage::Okay(ast::Tree::Unary(
                ast::Prefix::Bang,
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
            )),
        );
    }

    #[test]
    fn times() {
        assert_eq!(
            parse("A * B".chars()),
            Triage::Okay(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
            )),
        );
    }

    #[test]
    fn plus_times_no_paren() {
        assert_eq!(
            parse("A + B * C".chars()),
            Triage::Okay(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                ast::Infix::Plus,
                Box::new(ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
                )),
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
                    Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap()))
                ),
                Spanned {
                    msg: parse::Warning::UnnecessarySpace,
                    index: 1,
                },
            ),
        );
    }

    #[test]
    fn unnecessary_parens() {
        assert_eq!(
            parse("A + (B * C)".chars()),
            Triage::Warn(
                ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                    ast::Infix::Plus,
                    Box::new(ast::Tree::Binary(
                        Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                        ast::Infix::Times,
                        Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
                    )),
                ),
                Spanned {
                    msg: parse::Warning::UnnecessaryParens,
                    index: 4,
                },
            ),
        );
    }

    #[test]
    fn double_parens() {
        assert_eq!(
            parse("((A + B)) * C".chars()),
            Triage::Warn(
                ast::Tree::Binary(
                    Box::new(ast::Tree::Binary(
                        Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                        ast::Infix::Plus,
                        Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                    )),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
                ),
                Spanned {
                    msg: parse::Warning::UnnecessaryParens,
                    index: 1,
                },
            ),
        );
    }

    #[test]
    fn multiple_leading_spaces() {
        assert_eq!(
            parse("   A".chars()),
            Triage::Warn(
                ast::Tree::Value(Name::from_char('A', 0).strict().unwrap()),
                Spanned {
                    msg: parse::Warning::LeadingSpace,
                    index: 0,
                },
            ),
        );
    }

    #[test]
    fn multiple_trailing_spaces() {
        assert_eq!(
            parse("A   ".chars()),
            Triage::Warn(
                ast::Tree::Value(Name::from_char('A', 0).strict().unwrap()),
                Spanned {
                    msg: parse::Warning::TrailingSpace,
                    index: usize::MAX,
                },
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
            Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
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
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
            )),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
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
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
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
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                ast::Infix::Plus,
                Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
            )),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "(A + B) * C");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_5() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
            ast::Infix::Times,
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
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
                Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!(A * (B * C))");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_7() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
            ast::Infix::Plus,
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                ast::Infix::With,
                Box::new(ast::Tree::Binary(
                    Box::new(ast::Tree::Value(Name::from_char('C', 0).strict().unwrap())),
                    ast::Infix::Par,
                    Box::new(ast::Tree::Value(Name::from_char('D', 0).strict().unwrap())),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "A + B & C @ D");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_8() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
            ast::Infix::Lollipop,
            Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "A -> B");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_1() {
        assert_eq!(
            parse("A ".chars()),
            Triage::Warn(
                ast::Tree::Value(Name::from_char('A', 0).strict().unwrap()),
                Spanned {
                    msg: parse::Warning::TrailingSpace,
                    index: usize::MAX,
                },
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
                    Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                ),
                Spanned {
                    msg: parse::Warning::MissingInfixSpace,
                    index: 1,
                },
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
                    Box::new(ast::Tree::Value(Name::from_char('A', 0).strict().unwrap())),
                    ast::Infix::Times,
                    Box::new(ast::Tree::Value(Name::from_char('B', 0).strict().unwrap())),
                ),
                Spanned {
                    msg: parse::Warning::MissingInfixSpace,
                    index: 3,
                },
            ),
        );
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_4() {
        assert_eq!(
            parse("(A)".chars()),
            Triage::Warn(
                ast::Tree::Value(Name::from_char('A', 0).strict().unwrap()),
                Spanned {
                    msg: parse::Warning::UnnecessaryParens,
                    index: 0, // TODO: arguable, could be 1
                },
            ),
        );
    }
}

#[cfg(feature = "quickcheck")] // not actually necessary but kindred spirits: please don't run miri on this
mod systematic {
    use crate::Spanned;

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
    #[allow(unreachable_code, unused_mut, unused_variables)] // FIXME
    fn exhaustive_and_valid_iff_both_unique_and_shortest() {
        const MAX_LEN: usize = 13;
        let mut v = vec![0];
        let mut exhaustive: std::collections::HashMap<ast::Tree, bool> =
            ast::Tree::exhaustive_to_length(MAX_LEN, false)
                .into_iter()
                .filter_map(|t| (format!("{t}").len() <= MAX_LEN).then_some((t, false)))
                .collect();
        assert_eq!(
            exhaustive.values().copied().collect::<Vec<_>>(),
            core::iter::repeat(false)
                .take(exhaustive.len())
                .collect::<Vec<_>>(),
        );
        println!("{:#?}", {
            let mut keys = exhaustive
                .keys()
                .map(|t| format!("{t}"))
                .collect::<Vec<_>>();
            keys.sort();
            keys
        });
        'check: loop {
            let chars = v.iter().map(|b| char::from(Character::from(*b)));
            #[allow(clippy::wildcard_enum_match_arm)]
            match parse(chars.clone()) {
                Triage::Okay(parsed) => {
                    #[allow(clippy::from_iter_instead_of_collect)]
                    exhaustive.get_mut(&parsed).map_or_else(
                        || {
                            panic!(
                                "Produced a value that should have needed \
                                more characters: \"{parsed}\" i.e. {parsed:?}",
                            );
                        },
                        |b| {
                            assert!(
                                !*b,
                                "Fully valid string \"{}\" parsed \
                                as an expression we've already seen",
                                String::from_iter(chars),
                            );
                            *b = true;
                        },
                    );
                }
                Triage::Warn(
                    parsed,
                    Spanned {
                        msg: parse::Warning::UnnecessaryParens,
                        index,
                    },
                ) => {
                    // must not be the shortest way to write it,
                    // or else the parens would have been necessary
                    #[allow(clippy::from_iter_instead_of_collect)]
                    exhaustive.get(&parsed).map_or_else(
                        || {
                            panic!(
                                "Produced a value that should have needed \
                                more characters: \"{parsed}\" i.e. {parsed:?}",
                            )
                        },
                        |seen| {
                            assert!(
                                *seen,
                                "Parentheses flagged as unnecessary in \"{}\" at character #{index}, \
                                but there's no shorter way to write `{parsed:?}`",
                                String::from_iter(v.iter().map(|b| char::from(Character::from(*b)))),
                            );
                        },
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
                for (tree, produced) in exhaustive {
                    assert!(produced, "Couldn't produce \"{tree}\" i.e. `{tree:?}`",);
                }
                return;
            }
        }
    }
}
