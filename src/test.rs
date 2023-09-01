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
            Triage::Error(parse::Error::EmptyExpression)
        );
    }

    #[test]
    fn empty_parens() {
        assert_eq!(
            parse("()".chars()),
            Triage::Error(parse::Error::EmptyExpression)
        );
    }

    #[test]
    fn name() {
        assert_eq!(
            parse([b'A']),
            Triage::Okay(ast::Tree::Value(Name::from_char('A').strict().unwrap()))
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

        // TODO: re-enable after the above passes
        // fn roundtrip_bytes_expr(bytes: Vec<u8>) -> quickcheck::TestResult {
        //     let Triage::Okay(parsed) = parse(bytes.iter().copied()) else { return quickcheck::TestResult::discard(); };
        //     let printed = format!("{parsed}");
        //     quickcheck::TestResult::from_bool(printed.chars().eq(bytes))
        // }
    }
}

mod reduced {
    use super::*;

    #[test]
    fn roundtrip_1() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
        );
        let printed = format!("{tree}");
        // assert_eq!(printed, "A * A");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_2() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Unary(
                ast::Prefix::Bang,
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            )),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
        );
        let printed = format!("{tree}");
        // assert_eq!(printed, "!A * A");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_3() {
        let tree = ast::Tree::Unary(
            ast::Prefix::Bang,
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                ast::Infix::Times,
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            )),
        );
        let printed = format!("{tree}");
        // assert_eq!(printed, "!(A * A)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }

    #[test]
    fn roundtrip_4() {
        let tree = ast::Tree::Binary(
            Box::new(ast::Tree::Binary(
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
                ast::Infix::Plus,
                Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
            )),
            ast::Infix::Times,
            Box::new(ast::Tree::Value(Name::from_char('A').strict().unwrap())),
        );
        let printed = format!("{tree}");
        assert_eq!(printed, "((A + A) * A)");
        let parsed = parse(printed.chars());
        assert_eq!(parsed, Triage::Okay(tree));
    }
}
