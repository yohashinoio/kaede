use inkwell::{context::Context, module::Module, OptimizationLevel};

use kaede_lex::lex;
use kaede_parse::parse;

use super::*;

type TestFunc = unsafe extern "C" fn() -> i32;

// Expects that a test function of type TestFunc is defined in the module.
fn jit_compile_test(module: &Module) -> i32 {
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    unsafe { ee.get_function::<TestFunc>("test").unwrap().call() }
}

fn cg_test(program: &str) -> anyhow::Result<i32> {
    let context = Context::create();
    let module = context.create_module("test");

    codegen(&context, &module, parse(lex(program))?)?;

    Ok(jit_compile_test(&module))
}

#[test]
fn add() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return 48 + 10 }")?, 58);

    Ok(())
}

#[test]
fn sub() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return 68 - 10 }")?, 58);

    Ok(())
}

#[test]
fn mul() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return 48 * 10 }")?, 480);

    Ok(())
}

#[test]
fn div() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return 580 / 10 }")?, 58);

    Ok(())
}

#[test]
fn mul_precedence() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return 48 + 10 * 2 }")?, 68);

    Ok(())
}

#[test]
fn div_precedence() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return 48 + 20 / 2 }")?, 58);

    Ok(())
}

#[test]
fn four_arithmetic_precedence() -> anyhow::Result<()> {
    assert_eq!(
        cg_test("fn test() i32 { return (48 -10/ 2) + 58 * 2 }")?,
        159
    );

    Ok(())
}

#[test]
fn unary_plus_and_minus() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return +(-(-58)) }")?, 58);

    Ok(())
}

#[test]
fn empty_function() -> anyhow::Result<()> {
    let program = r"fn f() {
    }

    fn test() i32 {
        f()
        return 58
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn return_stmt() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() i32 { return (48*2 +10 * 2) / 2}")?, 58);

    Ok(())
}

#[test]
fn variable() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let yoha = 48
        let io = 10
        return yoha + io
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn call_func() -> anyhow::Result<()> {
    let program = r"fn f1() i32 {
        return 48
    }

    fn f2() i32 {
        return 10
    }

    fn test() i32 {
        return f1() + f2()
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn fn_params() -> anyhow::Result<()> {
    let program = r"fn f(n i32) i32 {
        return n
    }

    fn test() i32 {
        return 58
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn fn_call_one_arg() -> anyhow::Result<()> {
    let program = r"fn f(n i32) i32 {
        return n
    }

    fn test() i32 {
        return f(58)
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn fn_call_multi_args() -> anyhow::Result<()> {
    let program = r"fn f(x i32, y i32) i32 {
        return x + y
    }

    fn test() i32 {
        return f(48, 10)
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn simple_if_statement() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        if 58 == 58 {
            return 58
        }

        return 123
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn if_else_statement() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        if 48 == 10 {
            return 48
        } else if
        58 == 58 {
            return 58
        } else
        {
            return 10
        }

        return 123
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn equality_operation() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        if 4810 == 4810 {
            return 58
        }

        return 123
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn loop_statement() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        loop {
            let mut n = 0

            if n == 58 {
                return 58
            }

            ++n
        }
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}
