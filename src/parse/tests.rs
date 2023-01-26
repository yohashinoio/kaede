use crate::{
    ast::ast::{Function, Top},
    lex::lex,
};

use super::*;

#[test]
fn function() -> anyhow::Result<()> {
    let expect = vec![Top::Function(Function {
        name: "f".to_string(),
    })];

    assert_eq!(parse(lex("fn f() {}"))?, expect);

    Ok(())
}
