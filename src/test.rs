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
    ast::{self, Atomic},
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
            parse("a".chars()),
            Triage::Okay(ast::Unsimplified::Atomic(
                Atomic::from_char('a', 0).strict().unwrap()
            )),
        );
    }

    #[test]
    fn bang() {
        assert_eq!(
            parse("!a".chars()),
            Triage::Okay(ast::Unsimplified::Unary(
                ast::UnsimplifiedPrefix::Bang,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap()
                )),
            )),
        );
    }

    #[test]
    fn times() {
        assert_eq!(
            parse("a * b".chars()),
            Triage::Okay(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap()
                )),
                ast::UnsimplifiedInfix::Times,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('b', 0).strict().unwrap()
                )),
            )),
        );
    }

    #[test]
    fn plus_times_no_paren() {
        assert_eq!(
            parse("a + b * c".chars()),
            Triage::Okay(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap()
                )),
                ast::UnsimplifiedInfix::Plus,
                Box::new(ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('b', 0).strict().unwrap()
                    )),
                    ast::UnsimplifiedInfix::Times,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('c', 0).strict().unwrap()
                    )),
                )),
            )),
        );
    }

    #[test]
    fn unnecessary_space() {
        assert_eq!(
            parse("! a".chars()),
            Triage::Warn(
                ast::Unsimplified::Unary(
                    ast::UnsimplifiedPrefix::Bang,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('a', 0).strict().unwrap()
                    ))
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
            parse("a + (b * c)".chars()),
            Triage::Warn(
                ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('a', 0).strict().unwrap()
                    )),
                    ast::UnsimplifiedInfix::Plus,
                    Box::new(ast::Unsimplified::Binary(
                        Box::new(ast::Unsimplified::Atomic(
                            Atomic::from_char('b', 0).strict().unwrap()
                        )),
                        ast::UnsimplifiedInfix::Times,
                        Box::new(ast::Unsimplified::Atomic(
                            Atomic::from_char('c', 0).strict().unwrap()
                        )),
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
            parse("((a + b)) * c".chars()),
            Triage::Warn(
                ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Binary(
                        Box::new(ast::Unsimplified::Atomic(
                            Atomic::from_char('a', 0).strict().unwrap()
                        )),
                        ast::UnsimplifiedInfix::Plus,
                        Box::new(ast::Unsimplified::Atomic(
                            Atomic::from_char('b', 0).strict().unwrap()
                        )),
                    )),
                    ast::UnsimplifiedInfix::Times,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('c', 0).strict().unwrap()
                    )),
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
            parse("   a".chars()),
            Triage::Warn(
                ast::Unsimplified::Atomic(Atomic::from_char('a', 0).strict().unwrap()),
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
            parse("a   ".chars()),
            Triage::Warn(
                ast::Unsimplified::Atomic(Atomic::from_char('a', 0).strict().unwrap()),
                Spanned {
                    msg: parse::Warning::TrailingSpace,
                    index: usize::MAX,
                },
            ),
        );
    }

    #[test]
    fn multi_char_name() {
        assert_eq!(
            parse("abcdefg".chars()),
            Triage::Okay(ast::Unsimplified::Atomic(
                Atomic::from_str("abcdefg").strict().unwrap()
            ))
        );
    }

    #[test]
    #[allow(clippy::non_ascii_literal)]
    fn non_ascii_name() {
        assert_eq!(
            parse("garçon".chars()),
            Triage::Okay(ast::Unsimplified::Atomic(
                Atomic::from_str("gar\u{e7}on").strict().unwrap() // gar{c cedille}on
            ))
        );
    }
}

#[cfg(feature = "quickcheck")]
mod prop {
    use super::*;

    quickcheck::quickcheck! {
        fn roundtrip_expr_bytes(tree: ast::Unsimplified) -> bool {
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

        fn roundtrip_simplified_unsimplified(orig: ast::Simplified) -> bool {
            ast::Unsimplified::from(orig.clone()).simplify() == orig
        }

        fn roundtrip_simplified_funky(orig: ast::Simplified) -> bool {
            orig.clone().funk().simplify() == orig
        }

        fn roundtrip_funky_simplified(orig: ast::Funky) -> bool {
            orig.clone().simplify().funk() == orig
        }

        fn roundtrip_simplified_dual_dual(orig: ast::Simplified) -> bool {
            orig.clone().dual().dual() == orig
        }

        fn roundtrip_funky_dual_dual(orig: ast::Funky) -> bool {
            orig.clone().dual().dual() == orig
        }

        fn roundtrip_simplified_dual_funky_dual(orig: ast::Simplified) -> bool {
            orig.clone().dual().funk().dual().simplify() == orig
        }

        fn roundtrip_funky_dual_simplified_dual(orig: ast::Funky) -> bool {
            orig.clone().dual().simplify().dual().funk() == orig
        }

        fn all_roads_lead_to_simplified(orig: ast::Unsimplified) -> bool {
            orig.clone().simplify() == orig.funk().simplify()
        }

        fn all_roads_lead_to_funk(orig: ast::Unsimplified) -> bool {
            orig.clone().funk() == orig.simplify().funk()
        }

        fn removing_spaces_doesnt_create_other_errors(orig: ast::Unsimplified, v: Vec<bool>) -> bool {
            let mut v = v; // quickcheck necessity
            let mut s = format!("{orig}");
            s.retain(|c| (c != ' ') || v.pop().unwrap_or(false));
            matches!(parse(s.chars()), Triage::Okay(_) | Triage::Warn(_, Spanned { msg: parse::Warning::MissingInfixSpace, .. }))
        }
    }
}

mod reduced {
    use super::*;

    #[test]
    fn roundtrip_expr_bytes_1() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('a', 0).strict().unwrap(),
            )),
            ast::UnsimplifiedInfix::Times,
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('b', 0).strict().unwrap(),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "a * b");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_2() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Unary(
                ast::UnsimplifiedPrefix::Bang,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap(),
                )),
            )),
            ast::UnsimplifiedInfix::Times,
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('b', 0).strict().unwrap(),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!a * b");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_3() {
        let tree = ast::Unsimplified::Unary(
            ast::UnsimplifiedPrefix::Bang,
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap(),
                )),
                ast::UnsimplifiedInfix::Times,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('b', 0).strict().unwrap(),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!(a * b)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_4() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap(),
                )),
                ast::UnsimplifiedInfix::Plus,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('b', 0).strict().unwrap(),
                )),
            )),
            ast::UnsimplifiedInfix::Times,
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('c', 0).strict().unwrap(),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "(a + b) * c");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_5() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('a', 0).strict().unwrap(),
            )),
            ast::UnsimplifiedInfix::Times,
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('b', 0).strict().unwrap(),
                )),
                ast::UnsimplifiedInfix::Times,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('c', 0).strict().unwrap(),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "a * (b * c)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_6() {
        let tree = ast::Unsimplified::Unary(
            ast::UnsimplifiedPrefix::Bang,
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap(),
                )),
                ast::UnsimplifiedInfix::Times,
                Box::new(ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('b', 0).strict().unwrap(),
                    )),
                    ast::UnsimplifiedInfix::Times,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('c', 0).strict().unwrap(),
                    )),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!(a * (b * c))");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_7() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('a', 0).strict().unwrap(),
            )),
            ast::UnsimplifiedInfix::Plus,
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('b', 0).strict().unwrap(),
                )),
                ast::UnsimplifiedInfix::With,
                Box::new(ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('c', 0).strict().unwrap(),
                    )),
                    ast::UnsimplifiedInfix::Par,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('d', 0).strict().unwrap(),
                    )),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "a + b & c @ d");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_8() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('a', 0).strict().unwrap(),
            )),
            ast::UnsimplifiedInfix::Lollipop,
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_char('b', 0).strict().unwrap(),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "a -> b");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_9() {
        let tree = ast::Unsimplified::Unary(
            ast::UnsimplifiedPrefix::Bang,
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(Atomic::Zero)),
                ast::UnsimplifiedInfix::Plus,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', usize::MAX).strict().unwrap(),
                )),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!(0 + a)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_10() {
        let tree = ast::Unsimplified::Binary(
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(Atomic::Zero)),
                ast::UnsimplifiedInfix::Plus,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', usize::MAX).strict().unwrap(),
                )),
            )),
            ast::UnsimplifiedInfix::With,
            Box::new(ast::Unsimplified::Atomic(Atomic::Zero)),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "(0 + a) & 0");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_expr_bytes_11() {
        let tree = ast::Unsimplified::Unary(
            ast::UnsimplifiedPrefix::Bang,
            Box::new(ast::Unsimplified::Atomic(
                Atomic::from_str("aa").strict().unwrap(),
            )),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "!aa");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    #[allow(unused_results)]
    fn roundtrip_bytes_expr_1() {
        assert_eq!(
            parse("a ".chars()),
            Triage::Warn(
                ast::Unsimplified::Atomic(Atomic::from_char('a', 0).strict().unwrap()),
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
            parse("a* b".chars()),
            Triage::Warn(
                ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('a', 0).strict().unwrap()
                    )),
                    ast::UnsimplifiedInfix::Times,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('b', 0).strict().unwrap()
                    )),
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
            parse("a *b".chars()),
            Triage::Warn(
                ast::Unsimplified::Binary(
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('a', 0).strict().unwrap()
                    )),
                    ast::UnsimplifiedInfix::Times,
                    Box::new(ast::Unsimplified::Atomic(
                        Atomic::from_char('b', 0).strict().unwrap()
                    )),
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
            parse("(a)".chars()),
            Triage::Warn(
                ast::Unsimplified::Atomic(Atomic::from_char('a', 0).strict().unwrap()),
                Spanned {
                    msg: parse::Warning::UnnecessaryParens,
                    index: 0, // TODO: arguable, could be 1
                },
            ),
        );
    }

    #[test]
    fn all_roads_lead_to_funk_1() {
        let orig = ast::Unsimplified::Unary(
            ast::UnsimplifiedPrefix::Dual,
            Box::new(ast::Unsimplified::Binary(
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap(),
                )),
                ast::UnsimplifiedInfix::Times,
                Box::new(ast::Unsimplified::Atomic(
                    Atomic::from_char('a', 0).strict().unwrap(),
                )),
            )),
        );
        println!(
            "\"{}\" ==> (\"{}\" =?= \"{}\")",
            orig,
            orig.clone().funk(),
            orig.clone().simplify().funk()
        );
        assert_eq!(orig.clone().funk(), orig.simplify().funk());
    }
}

#[cfg(feature = "systematic")]
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
                Character::A => 'a',
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
    #[allow(clippy::too_many_lines)]
    fn exhaustive_and_valid_iff_both_unique_and_shortest() {
        const MAX_LEN: usize = 13;
        let mut v = vec![0];
        let mut exhaustive: std::collections::HashMap<ast::Unsimplified, bool> =
            ast::Unsimplified::exhaustive_to_length(MAX_LEN, false)
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
                Triage::Okay(mut parsed) => {
                    if parsed.names().into_iter().all(|s| s.len() == 1) {
                        #[allow(clippy::from_iter_instead_of_collect)]
                        exhaustive.get_mut(&parsed).map_or_else(
                            || {
                                panic!(
                                    "Produced a value from \"{}\" that should have \
                                needed more characters: \"{parsed}\" i.e. {parsed:?}",
                                    chars.clone().collect::<String>(),
                                );
                            },
                            |b| {
                                assert!(
                                    !*b,
                                    "Fully valid string \"{}\" parsed \
                                as an expression we've already seen",
                                    String::from_iter(chars.clone()),
                                );
                                *b = true;
                            },
                        );
                    }
                }
                Triage::Warn(
                    mut parsed,
                    Spanned {
                        msg: parse::Warning::UnnecessaryParens,
                        index,
                    },
                ) => {
                    if parsed.names().into_iter().all(|s| s.len() == 1) {
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
                                    "Parentheses flagged as unnecessary in \
                                    \"{}\" at character #{index}, but \
                                    there's no shorter way to write `{parsed:?}`",
                                    String::from_iter(
                                        v.iter().map(|b| char::from(Character::from(*b)))
                                    ),
                                );
                            },
                        );
                    }
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
