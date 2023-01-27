use crate::{
    ast::ast::{Expr, Function, Top},
    lex::lex,
};

use super::*;

#[test]
fn function() -> anyhow::Result<()> {
    let expect = vec![Top::Function(Function {
        name: "f".to_string(),
        body: Expr::Integer(4810),
    })];

    assert_eq!(parse(lex("fn f() { 4810 }"))?, expect);

    Ok(())
}
