/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![allow(clippy::missing_const_for_fn, clippy::unwrap_used, unused_imports)]

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
}

#[cfg(feature = "quickcheck")]
mod prop {
    use super::*;

    quickcheck::quickcheck! {
        // TODO: re-enable before pushing, but this ironically takes the longest
        // fn roundtrip_expr_bytes(tree: ast::Tree) -> bool {
        //     let printed = format!("{tree}");
        //     let parsed = parse(printed.chars());
        //     parsed == Triage::Okay(tree)
        // }

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
        // assert_eq!(printed, "A * A");
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
        // assert_eq!(printed, "!A * A");
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
        // assert_eq!(printed, "!(A * A)");
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
        // assert_eq!(printed, "(A + B) * C");
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
                parse::Warning::MissingInfixSpace('*'),
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
                parse::Warning::MissingInfixSpace('B'),
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
                parse::Warning::UnnecessaryParentheses,
            ),
        );
    }
}
