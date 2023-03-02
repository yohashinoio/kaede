use kaede_ast::expr::{BinOpKind, Expr};
use kaede_ast::stmt::{Let, Return, Stmt, StmtList};
use kaede_ast::Top;
use kaede_lex::lex;
use kaede_location::{Location, Span};
use kaede_type::{FundamentalTypeKind, TypeEnum};

use super::*;

fn create_simple_binop(lhs: i32, kind: BinOpKind, rhs: i32) -> Expr {
    Expr::new_binop(
        Box::new(Expr::new_i32(lhs)),
        kind,
        Box::new(Expr::new_i32(rhs)),
    )
}

/// Function name is fixed to "test".
fn parse_test(program: &str, fn_body: StmtList) -> anyhow::Result<()> {
    assert_eq!(
        parse(lex(program))?,
        TranslationUnit::from([Top::Function {
            name: "test".to_string(),
            body: fn_body,
        }])
    );

    Ok(())
}

#[test]
fn add() -> anyhow::Result<()> {
    let body = StmtList::from([Stmt::Expr(create_simple_binop(48, BinOpKind::Add, 10))]);

    parse_test("fn test() { 48 + 10; }", body)?;

    Ok(())
}

#[test]
fn sub() -> anyhow::Result<()> {
    let body = StmtList::from([Stmt::Expr(create_simple_binop(48, BinOpKind::Sub, 10))]);

    parse_test("fn test() { 48 - 10 }", body)?;

    Ok(())
}

#[test]
fn mul() -> anyhow::Result<()> {
    let body = StmtList::from([Stmt::Expr(create_simple_binop(48, BinOpKind::Mul, 10))]);

    parse_test("fn test() { 48 * 10 }", body)?;

    Ok(())
}

#[test]
fn div() -> anyhow::Result<()> {
    let body = StmtList::from([Stmt::Expr(create_simple_binop(48, BinOpKind::Div, 10))]);

    parse_test("fn test() { 48 / 10 }", body)?;

    Ok(())
}

#[test]
fn mul_precedence() -> anyhow::Result<()> {
    let body = StmtList::from([Stmt::Expr(Expr::new_binop(
        Box::new(Expr::new_i32(48)),
        BinOpKind::Add,
        Box::new(create_simple_binop(10, BinOpKind::Mul, 5)),
    ))]);

    parse_test("fn test() { 48 + 10 * 5 }", body)?;

    Ok(())
}

#[test]
fn div_precedence() -> anyhow::Result<()> {
    let body = StmtList::from([Stmt::Expr(Expr::new_binop(
        Box::new(Expr::new_i32(48)),
        BinOpKind::Sub,
        Box::new(create_simple_binop(10, BinOpKind::Div, 5)),
    ))]);

    parse_test("fn test() { 48 - 10 / 5 }", body)?;

    Ok(())
}

#[test]
fn four_arithmetic_precedence() -> anyhow::Result<()> {
    let tmp1 = Expr::new_binop(
        Box::new(Expr::new_i32(48)),
        BinOpKind::Add,
        Box::new(create_simple_binop(10, BinOpKind::Div, 5)),
    );

    let tmp2 = Expr::new_binop(
        Box::new(Expr::new_i32(58)),
        BinOpKind::Mul,
        Box::new(Expr::new_i32(2)),
    );

    let body = StmtList::from([Stmt::Expr(Expr::new_binop(
        Box::new(tmp1),
        BinOpKind::Sub,
        Box::new(tmp2),
    ))]);

    parse_test("fn test() { (48 +10/ 5) - 58 * 2;}", body)?;

    Ok(())
}

#[test]
fn unary_plus_and_minus() -> anyhow::Result<()> {
    let create_unary_minus =
        |n| Expr::new_binop(Box::new(Expr::new_i32(0)), BinOpKind::Sub, Box::new(n));

    parse_test(
        "fn test() { +(-(-58)) }",
        StmtList::from([Stmt::Expr(create_unary_minus(create_unary_minus(
            Expr::new_i32(58),
        )))]),
    )?;

    Ok(())
}

#[test]
fn function() -> anyhow::Result<()> {
    parse_test(
        "fn test() { 4810 }",
        StmtList::from([Stmt::Expr(Expr::new_i32(4810))]),
    )?;

    Ok(())
}

#[test]
fn empty_function() -> anyhow::Result<()> {
    parse_test("fn test() {}", StmtList::new())?;

    Ok(())
}

#[test]
fn return_stmt() -> anyhow::Result<()> {
    parse_test(
        "fn test() { return 4810 }",
        StmtList::from([Stmt::Return(Return(Some(Expr::new_i32(4810))))]),
    )?;

    Ok(())
}

#[test]
fn return_stmt_with_no_value() -> anyhow::Result<()> {
    parse_test(
        "fn test() { return }",
        StmtList::from([Stmt::Return(Return(None))]),
    )?;

    Ok(())
}

#[test]
fn error_span() {
    let r = parse(lex("fn () { 4810; }")).expect_err("Expected Err, but it was OK.");

    match r {
        ParseError::ExpectedError {
            expected: _,
            but: _,
            span,
        } => {
            assert_eq!(
                span,
                Span::new(
                    Location { line: 1, column: 4 },
                    Location { line: 1, column: 5 }
                )
            )
        }

        _ => panic!(),
    }
}

#[test]
fn statement_list() -> anyhow::Result<()> {
    parse_test(
        "fn test() { 48\n 10\n return }",
        StmtList::from([
            Stmt::Expr(Expr::new_i32(48)),
            Stmt::Expr(Expr::new_i32(10)),
            Stmt::Return(Return(None)),
        ]),
    )?;

    Ok(())
}

#[test]
fn variable() -> anyhow::Result<()> {
    parse_test(
        "fn test() { let yoha\n let io\n return }",
        StmtList::from([
            Stmt::Let(Let {
                name: "yoha".to_string(),
                init: None,
                ty: TypeEnum::new_fundamental_type(FundamentalTypeKind::I32),
            }),
            Stmt::Let(Let {
                name: "io".to_string(),
                init: None,
                ty: TypeEnum::new_fundamental_type(FundamentalTypeKind::I32),
            }),
            Stmt::Return(Return(None)),
        ]),
    )?;

    Ok(())
}

#[test]
fn variable_initialization() -> anyhow::Result<()> {
    parse_test(
        "fn test() { let yoha = 48\n let io = 10\n }",
        StmtList::from([
            Stmt::Let(Let {
                name: "yoha".to_string(),
                init: Some(Expr::new_i32(48)),
                ty: TypeEnum::new_fundamental_type(FundamentalTypeKind::I32),
            }),
            Stmt::Let(Let {
                name: "io".to_string(),
                init: Some(Expr::new_i32(10)),
                ty: TypeEnum::new_fundamental_type(FundamentalTypeKind::I32),
            }),
        ]),
    )?;

    Ok(())
}

#[test]
fn string_literal() -> anyhow::Result<()> {
    parse_test(
        r#"fn test() { "よはいお" }"#,
        StmtList::from([Stmt::Expr(Expr::StringLiteral("よはいお".to_string()))]),
    )?;
}
