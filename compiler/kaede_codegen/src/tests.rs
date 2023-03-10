use inkwell::{context::Context, module::Module, OptimizationLevel};

use kaede_lex::lex;
use kaede_parse::parse;

use super::*;

type TestFunc = unsafe extern "C" fn() -> i32;

// Expects that a test function of type TestFunc is defined in the module
fn jit_compile_test(module: &Module) -> i32 {
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    unsafe { ee.get_function::<TestFunc>("test").unwrap().call() }
}

fn run_test(program: &str) -> CodegenResult<i32> {
    let context = Context::create();
    let module = context.create_module("test");

    codegen(&context, &module, parse(lex(program)).unwrap())?;

    Ok(jit_compile_test(&module))
}

#[test]
fn add() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return 48 + 10 }")?, 58);

    Ok(())
}

#[test]
fn sub() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return 68 - 10 }")?, 58);

    Ok(())
}

#[test]
fn mul() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return 48 * 10 }")?, 480);

    Ok(())
}

#[test]
fn div() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return 580 / 10 }")?, 58);

    Ok(())
}

#[test]
fn mul_precedence() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return 48 + 10 * 2 }")?, 68);

    Ok(())
}

#[test]
fn div_precedence() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return 48 + 20 / 2 }")?, 58);

    Ok(())
}

#[test]
fn four_arithmetic_precedence() -> anyhow::Result<()> {
    assert_eq!(
        run_test("fn test() i32 { return (48 -10/ 2) + 58 * 2 }")?,
        159
    );

    Ok(())
}

#[test]
fn unary_plus_and_minus() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return +(-(-58)) }")?, 58);

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

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn return_stmt() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() i32 { return (48*2 +10 * 2) / 2}")?, 58);

    Ok(())
}

#[test]
fn empty_return_stmt() -> anyhow::Result<()> {
    let program = r"fn f() {
        return
    }

    fn test() i32 {
        f()
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn let_statement() -> anyhow::Result<()> {
    // Type inference
    let program = r"fn test() i32 {
        let yoha = 48
        let io = 10
        return yoha + io
    }";

    assert_eq!(run_test(program)?, 58);

    // Mutable, Type inference
    let program = r"fn test() i32 {
        let mut yohaio = 58
        return yohaio
    }";

    assert_eq!(run_test(program)?, 58);

    // Specified type
    let program = r"fn test() i32 {
        let yohaio i32 = 58
        return yohaio
    }";

    assert_eq!(run_test(program)?, 58);

    // Mutable, Specified type
    let program = r"fn test() i32 {
        let mut yohaio i32 = 58
        return yohaio
    }";

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

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

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn loop_statement() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let mut n = 0

        loop {
            if n == 58 {
                break
            }

            n = n + 1
        }

        return n
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn break_statement() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        loop {
            break
        }

        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn break_outside_of_loop() {
    let program = r"fn test() i32 {
        break
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::BreakOutsideOfLoop { .. })
    ));
}

#[test]
fn simple_assignment() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let mut n = 0

        n = 58

        return n
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable() {
    let program = r"fn test() i32 {
        let n = 58
        n = 4810
        return n
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignTwiceToImutable { .. })
    ));
}

#[test]
fn string_literal() -> anyhow::Result<()> {
    let program = r#"fn test() i32 {
        let s1 = "yohaio"
        let s2 = "????????????"

        return 58
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn define_struct() -> anyhow::Result<()> {
    let program = r"struct A {
        a i32
        b bool
    }

    fn test() i32 {
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn use_struct() -> anyhow::Result<()> {
    let program = r"struct Person {
        age i32
        stature i32
        is_male bool
        is_female bool
    }

    fn test() i32 {
        let person = Person { is_male false, stature 48, age 10, is_female true }
        return person.age + person.stature
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn true_() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let b = true

        if b {
            return 58
        }

        return 123
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn false_() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let b = false

        if b {
            return 123
        }

        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn has_no_fields() {
    let program = r"fn test() i32 {
        4810.shino
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::HasNoFields { .. })
    ));
}

#[test]
fn shared_borrow() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let s = 58
        let r = &s
        return *r
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn reference_type_argument() -> anyhow::Result<()> {
    let program = r"fn dref(r &i32) i32 {
        return *r
    }

    fn test() i32 {
        let n = 58
        return dref(&n)
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn mutable_borrow_and_deref() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let mut s = 58
        let m = &mut s
        *m = 58
        return s
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn borrow() -> anyhow::Result<()> {
    let program = r"fn to_58(n &mut i32) {
        *n = 58
    }

    fn test() i32 {
        let mut a = 123

        to_58(&mut a)

        return a
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_reference() {
    let program = r"fn test() i32 {
        let mut n = 58

        let r = &n;

        *r = 123

        return n
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignToImutableRef { .. })
    ));
}

#[test]
fn borrow_temporary_value() -> anyhow::Result<()> {
    let program = r"fn test() i32 {
        let r = &58

        return *r
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}
