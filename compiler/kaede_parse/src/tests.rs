use kaede_ast::{BinOpKind, Expr, Top};
use kaede_lex::lex;

use super::*;

fn create_simple_binop(lhs: u64, kind: BinOpKind, rhs: u64) -> Expr {
    use Expr::*;

    BinOp(Box::new(Integer(lhs)), kind, Box::new(Integer(rhs)))
}

/// Function name is fixed to "test"
fn create_test_fn(body: Expr) -> Top {
    Top::Function {
        name: "test".to_string(),
        body,
    }
}

#[test]
fn add() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 48 + 10 }"))?,
        TranslationUnit::from([create_test_fn(create_simple_binop(48, BinOpKind::Add, 10))])
    );

    Ok(())
}

#[test]
fn sub() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 48 - 10 }"))?,
        TranslationUnit::from([create_test_fn(create_simple_binop(48, BinOpKind::Sub, 10))])
    );

    Ok(())
}

#[test]
fn mul() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 48 * 10 }"))?,
        TranslationUnit::from([create_test_fn(create_simple_binop(48, BinOpKind::Mul, 10))])
    );

    Ok(())
}

#[test]
fn div() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 48 / 10 }"))?,
        TranslationUnit::from([create_test_fn(create_simple_binop(48, BinOpKind::Div, 10))])
    );

    Ok(())
}

#[test]
fn mul_precedence() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 48 + 10 * 5 }"))?,
        TranslationUnit::from([create_test_fn(Expr::BinOp(
            Box::new(Expr::Integer(48)),
            BinOpKind::Add,
            Box::new(create_simple_binop(10, BinOpKind::Mul, 5))
        ))])
    );

    Ok(())
}

#[test]
fn div_precedence() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 48 - 10 / 5 }"))?,
        TranslationUnit::from([create_test_fn(Expr::BinOp(
            Box::new(Expr::Integer(48)),
            BinOpKind::Sub,
            Box::new(create_simple_binop(10, BinOpKind::Div, 5))
        ))])
    );

    Ok(())
}

#[test]
fn four_arithmetic_precedence() -> anyhow::Result<()> {
    let tmp1 = Expr::BinOp(
        Box::new(Expr::Integer(48)),
        BinOpKind::Add,
        Box::new(create_simple_binop(10, BinOpKind::Div, 5)),
    );

    let tmp2 = Expr::BinOp(
        Box::new(Expr::Integer(58)),
        BinOpKind::Mul,
        Box::new(Expr::Integer(2)),
    );

    assert_eq!(
        parse(lex(r" fn test() { (48 +10/ 5) - 58 * 2}"))?,
        TranslationUnit::from([create_test_fn(Expr::BinOp(
            Box::new(tmp1),
            BinOpKind::Sub,
            Box::new(tmp2)
        ))])
    );

    Ok(())
}

#[test]
fn unary_plus_and_minus() -> anyhow::Result<()> {
    let create_unary_minus =
        |n| Expr::BinOp(Box::new(Expr::Integer(0)), BinOpKind::Sub, Box::new(n));

    assert_eq!(
        parse(lex("fn test() { +(-(-58)) }"))?,
        TranslationUnit::from([create_test_fn(create_unary_minus(create_unary_minus(
            Expr::Integer(58)
        )))])
    );

    Ok(())
}

#[test]
fn function() -> anyhow::Result<()> {
    assert_eq!(
        parse(lex("fn test() { 4810 }"))?,
        TranslationUnit::from([create_test_fn(Expr::Integer(4810))])
    );

    Ok(())
}
