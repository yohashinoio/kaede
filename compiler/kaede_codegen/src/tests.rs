use inkwell::{
    context::Context, module::Module, support::load_library_permanently, OptimizationLevel,
};

use kaede_lex::lex;
use kaede_parse::parse;

use super::*;

// Expects that a test function of type TestFunc is defined in the module
fn jit_compile_test(module: &Module) -> i32 {
    // Load bdw-gc (boehm-gc)
    load_library_permanently("/usr/local/lib/libgc.so");

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i32>("test.test")
            .unwrap()
            .call()
    }
}

fn run_test(program: &str) -> CodegenResult<i32> {
    let context = Context::create();
    let module = context.create_module("test");

    let cgcx = CodegenContext::new(&context)?;

    codegen(
        &cgcx,
        &module,
        PathBuf::from("test"),
        parse(lex(program)).unwrap(),
        OptimizationLevel::None,
    )?;

    Ok(jit_compile_test(&module))
}

#[test]
fn add() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return 48 + 10 }")?, 58);

    Ok(())
}

#[test]
fn sub() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return 68 - 10 }")?, 58);

    Ok(())
}

#[test]
fn mul() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return 48 * 10 }")?, 480);

    Ok(())
}

#[test]
fn div() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return 580 / 10 }")?, 58);

    Ok(())
}

#[test]
fn mul_precedence() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return 48 + 10 * 2 }")?, 68);

    Ok(())
}

#[test]
fn div_precedence() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return 48 + 20 / 2 }")?, 58);

    Ok(())
}

#[test]
fn four_arithmetic_precedence() -> anyhow::Result<()> {
    assert_eq!(
        run_test("fn test() -> i32 { return (48 -10/ 2) + 58 * 2 }")?,
        159
    );

    Ok(())
}

#[test]
fn unary_plus_and_minus() -> anyhow::Result<()> {
    assert_eq!(run_test("fn test() -> i32 { return +(-(-58)) }")?, 58);

    Ok(())
}

#[test]
fn empty_function() -> anyhow::Result<()> {
    let program = r"fn f() {
    }

    fn test() -> i32 {
        f()
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn return_() -> anyhow::Result<()> {
    assert_eq!(
        run_test("fn test() -> i32 { return (48*2 +10 * 2) / 2}")?,
        58
    );

    Ok(())
}

#[test]
fn empty_return() -> anyhow::Result<()> {
    let program = r"fn f() {
        return
    }

    fn test() -> i32 {
        f()
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn let_statement() -> anyhow::Result<()> {
    // Type inference
    let program = r"fn test() -> i32 {
        let yoha = 48
        let io = 10
        return yoha + io
    }";

    assert_eq!(run_test(program)?, 58);

    // Mutable, Type inference
    let program = r"fn test() -> i32 {
        let mut yohaio = 58
        return yohaio
    }";

    assert_eq!(run_test(program)?, 58);

    // Specified type
    let program = r"fn test() -> i32 {
        let yohaio: i32 = 58
        return yohaio
    }";

    assert_eq!(run_test(program)?, 58);

    // Mutable, Specified type
    let program = r"fn test() -> i32 {
        let mut yohaio: i32 = 58
        return yohaio
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn call_function() -> anyhow::Result<()> {
    let program = r"fn f1() -> i32 {
        return 48
    }

    fn f2() -> i32 {
        return 10
    }

    fn test() -> i32 {
        return f1() + f2()
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn function_parameters() -> anyhow::Result<()> {
    let program = r"fn f(n: i32) -> i32 {
        return n
    }

    fn test() -> i32 {
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn function_call_with_one_argument() -> anyhow::Result<()> {
    let program = r"fn f(n: i32) -> i32 {
        return n
    }

    fn test() -> i32 {
        return f(58)
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn function_call_with_multi_args() -> anyhow::Result<()> {
    let program = r"fn f(x: i32, y: i32) -> i32 {
        return x + y
    }

    fn test() -> i32 {
        return f(48, 10)
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn simple_if() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if 58 == 58 {
            return 58
        }

        return 123
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_else() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
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
    let program = r"fn test() -> i32 {
        if 4810 == 4810 {
            return 58
        }

        return 123
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn loop_() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
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
fn break_() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
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
    let program = r"fn test() -> i32 {
        break
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::BreakOutsideOfLoop { .. })
    ));
}

#[test]
fn simple_assignment() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let mut n = 0

        n = 58

        return n
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable() {
    let program = r"fn test() -> i32 {
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
    let program = r#"fn test() -> i32 {
        let s1 = "yohaio"
        let s2 = "よはいお"

        return 58
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn string_type() -> anyhow::Result<()> {
    let program = r#"fn f(s: str) -> str {
        return s
    }

    fn test() -> i32 {
        let s: str = f("Yohaio")
        return 58
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn define_struct() -> anyhow::Result<()> {
    let program = r"struct A {
        a: i32,
        b: bool,
    }

    struct B { a: i32, b: bool }

    fn test() -> i32 {
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn struct_field_access() -> anyhow::Result<()> {
    let program = r"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn test() -> i32 {
        let person = Person { is_male: false, stature: 48, age: 10, is_female: true }
        return person.age + person.stature
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn true_() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
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
    let program = r"fn test() -> i32 {
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
    let program = r"fn test() -> i32 {
        4810.shino
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::HasNoFields { .. })
    ));
}

#[test]
fn less_than() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if 48 < 48 {
            return 123
        } else if 48 < 10 {
            return 124
        } else if 10 < 48 {
            return 58
        }

        return 125
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn greater_than() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if 10 > 10 {
            return 123
        } else if 10 > 48 {
            return 124
        } else if 48 > 10 {
            return 58
        }

        return 125
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn less_than_or_equal() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if 48 <= 48 {
            return 58
        } else {
            return 123
        }

        return 86
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn greater_than_or_equal() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if 10 >= 10 {
            return 58
        } else {
            return 123
        }

        return 86
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn not_equal_to() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if 48 != 48 {
            return 123
        } else if 48 != 10 {
            return 58
        }

        return 124
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn logical_not() -> anyhow::Result<()> {
    let program = r"fn lnot(fl: bool) -> bool {
        return !fl
    }

    fn test() -> i32 {
        if !(48 != 10) {
            return 123
        }

        let fls = false;

        if lnot(fls) {
            return 58
        }

        return 124
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn remainder() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        return 123 % 7
    }";

    assert_eq!(run_test(program)?, 4);

    Ok(())
}

#[test]
fn logical_or() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if false || false {
            return 123
        }
        if false || true {
            if true || false {
                if true || true {
                    if 48 < 10 || 48 != 10 {
                        return 58
                    }
                }
            }
        }
        return 124
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn logical_and() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        if false && true {
            return 123
        }
        if true && false {
            return 124
        }
        if false && false {
            return 125
        }
        if true && true {
            if 48 > 10 && 48 != 10 {
                return 58
            }
        }
        return 126
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn array_literal() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let a = [48, 10]
        return 58
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn array_indexing() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let a = [48, 10]
        return a[0] + [4][0] + a[1]
    }";

    assert_eq!(run_test(program)?, 62);

    Ok(())
}

#[test]
fn array_type() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let a: [i32; 2] = [48, 10]
        let n: [i32; 1] = [4]
        return a[0] + a[1] + n[0]
    }";

    assert_eq!(run_test(program)?, 62);

    Ok(())
}

#[test]
fn array_as_argument() -> anyhow::Result<()> {
    let program = r"fn add(a: [i32; 2]) -> i32 {
        return a[0] + a[1]
    }

    fn test() -> i32 {
        let a = [48, 10]

        return add(a)
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn array_as_mutable_argument() -> anyhow::Result<()> {
    let program = r"fn modify(mut a: [i32; 2]) {
        a[0] = 48
        a[1] = 10
    }

    fn add(a: [i32; 2]) -> i32 {
        return a[0] + a[1]
    }

    fn test() -> i32 {
        let mut a = [12, 34]

        modify(a)

        return add(a)
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn immutable_array_as_mutable_argument() {
    let program = r"fn modify(mut a: [i32; 2]) {
        a[0] = 48
        a[1] = 10
    }

    fn add(a: [i32; 2]) -> i32 {
        return a[0] + a[1]
    }

    fn test() -> i32 {
        let a = [123, 124]

        // ERROR!
        modify(a)

        return add(a)
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignImmutableToMutable { .. })
    ));
}

#[test]
fn assign_to_array_elements() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let mut a = [123]

        a[0] = 58

        return a[0]
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tuple_indexing() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let tup = (58, true, "hello")

        if tup.1 {
            return tup.0
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tuple_unpacking() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let tup = (48, true, "hello", 10)

        let (n1, f, _, n2) = tup

        if f {
            return n1 + n2
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    let program = r#"fn test() -> i32 {
        let (n1, f, _, n2) = (48, true, "hello", 10)

        if f {
            return n1 + n2
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tuple_as_argument() -> anyhow::Result<()> {
    let program = r"fn tup1(a: (i32, bool)) -> i32 {
        if a.1 {
            return a.0
        }

        return 123
    }

    fn tup2(a: (i32, bool)) -> i32 {
        let (n, f) = a

        if f {
            return n
        }

        return 124
    }

    fn test() -> i32 {
        let tup = (58, true)

        return tup1(tup) + tup2(tup)
    }";

    assert_eq!(run_test(program)?, 116);

    Ok(())
}

#[test]
fn tuple_as_mutable_argument() -> anyhow::Result<()> {
    let program = r"fn modify(mut tup: (i32, bool)) {
        tup.0 = 58
        tup.1 = true
    }

    fn test() -> i32 {
        let mut tup = (123, false)

        modify(tup)

        if tup.1 {
            return tup.0
        }

        return 123
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tuples_require_access_by_index() {
    let program = r"fn test() -> i32 {
        let tup = (58, true)
        tup.llvm
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::TupleRequireAccessByIndex { .. })
    ));
}

#[test]
fn tuple_in_tuple() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let mut tuptup = ((48, true), (0, true))

        let mut io = tuptup.1
        io.0 = 10

        return tuptup.0.0 + tuptup.1.0
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_struct_field() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn test() -> i32 {
        let mut person = Person { is_male: false, stature: 48, age: 0, is_female: true }
        person.age = 10
        return person.age + person.stature
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_struct_field() {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn test() -> i32 {
        let person = Person { is_male: false, stature: 48, age: 0, is_female: true }
        person.age = 10
        return person.age + person.stature
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignTwiceToImutable { .. })
    ));
}

#[test]
fn assign_to_tuple_field() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let mut t = (58, false)
        t.1 = true

        if t.1 {
            return t.0
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_tuple_field() {
    let program = r#"fn test() -> i32 {
        let t = (58, false)
        t.1 = true

        if t.1 {
            return t.0
        }

        return 123
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignTwiceToImutable { .. })
    ));
}

#[test]
fn comments() -> anyhow::Result<()> {
    let program = r"// hello, world
    /* hello, world */
    /*
    hello, world
    world, hello
     */
    fn test() -> i32 {
        // hello, world
        /* hello, world */
        /*
        hello, world
        world, hello
         */

        let n /* Number */ = 58 // Let

        return /* My name is John Smith */ n // Return

        // hello, world
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_1() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let x = if true {
            58
        } else {
            123
        }

        return x
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_2() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let x = if true {
            58
        } else {
            return 123
        }

        return x
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_3() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let x = if false {
            123
        } else {
            return 58
        }

        return x
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_to_initializers_4() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let n = 4810

        let x = if false {
            123
        } else if n == 4810 {
            58
        } else {
            return 123
        }

        return x
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_in_return_1() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        return if true {
            58
        } else {
            123
        }
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_in_return_2() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        return if false {
            123
        } else {
            return 58
        }
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_expr_in_return_3() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        return 122 + if true {
            return 58
        } else {
            1
        }
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn nested_if_expr_1() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let n = 4810

        let x = if n == 4810 {
            if false {
                123
            } else if true {
                58
            } else {
                return 124
            }
        } else if true {
            125
        } else {
            return 126
        }

        return x
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn nested_if_expr_2() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let n = 4810

        let x = if false {
            123
        } else if true {
            if false {
                124
            } else if n != 4810 {
                125
            } else {
                return 58
            }
        } else {
            return 126
        }

        return x
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn copy_struct() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn test() -> i32 {
        let mut p1 = Person {
            is_male: false,
            stature: 48,
            age: 0,
            is_female: true,
        }

        let mut p2 = p1

        p2.age = 10

        return p1.age + p1.stature
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn copy_array() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let mut ar = [128, 256, 512]

        let mut arr = ar

        arr[0] = 58

        return ar[0]
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn copy_tuple() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let mut tup = (false, 123)

        let mut tupp = tup

        tupp.0 = true
        tupp.1 = 58

        if tup.0 {
            return tup.1
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn struct_as_mutable_argument() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn f(mut p: Person) {
        p.age = 10
    }

    fn test() -> i32 {
        let mut p1 = Person {
            is_male: false,
            stature: 48,
            age: 0,
            is_female: true,
        }

        f(p1)

        return p1.age + p1.stature
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn struct_in_struct() -> anyhow::Result<()> {
    let program = r"struct Age {
       n: i32,
    }

    struct Person {
        age: Age,
    }

    fn test() -> i32 {
        let mut p = Person {
            age: Age { n: 0 },
        }

        let mut age = p.age
        age.n = 58

        return p.age.n
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn copy_scalar_type_data() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let n = 58

        let mut m = n
        m = 123

        return n
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_to_mutable() {
    // Array
    let program = r#"fn test() -> i32 {
        let ar = [128, 256, 512]

        let mut arr = ar

        return ar[0]
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignImmutableToMutable { .. })
    ));

    // Tuple
    let program = r#"fn test() -> i32 {
        let tup = (58, true)

        let mut t = tup

        return tup.0
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignImmutableToMutable { .. })
    ));

    // Struct
    let program = r#"struct Number {
        n: i32,
    }

    fn test() -> i32 {
        let n = Number { n: 58 }

        let mut m = n

        return n.n
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignImmutableToMutable { .. })
    ));
}

#[test]
fn return_struct() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    fn f() -> Person {
        return Person { age: 58 }
    }

    fn test() -> i32 {
        let s = f()

        return s.age
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn return_tuple() -> anyhow::Result<()> {
    let program = r#"fn f() -> (i32, bool) {
        return (58, true)
    }

    fn test() -> i32 {
        let t = f()

        if t.1 {
            return t.0
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn return_array() -> anyhow::Result<()> {
    let program = r#"fn f() -> [i32; 2] {
        return [48, 10]
    }

    fn test() -> i32 {
        let ar = f()

        return ar[0] + ar[1]
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn if_in_loop() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    fn get_age(p: Person) -> i32 {
        return p.age
    }

    fn test() -> i32 {
        let mut p = Person { age: 0 }

        loop {
            if get_age(p) == 58 {
                break
            } else {
                p.age = p.age + 1
            }
        }

        return get_age(p)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn simple_method() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        mt get_age() -> i32 {
            return self.age
        }
    }

    fn test() -> i32 {
        let mut p = Person { age: 123 }

        p.age = 58

        return p.get_age()
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn mutable_method() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        mt get_age() -> i32 {
            return self.age
        }

        mt mut change_age_to(new_age: i32) {
            self.age = new_age
        }
    }

    fn test() -> i32 {
        let mut p = Person { age: 123 }

        p.change_age_to(58)

        return p.get_age()
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn call_mutable_methods_from_immutable() {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        mt get_age() -> i32 {
            return self.age
        }

        mt mut change_age_to(new_age: i32) {
            self.age = new_age
        }
    }

    fn test() -> i32 {
        let p = Person { age: 123 }

        p.change_age_to(58)

        return p.get_age()
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignImmutableToMutable { .. })
    ));
}

#[test]
fn modify_fields_in_immutable_methods() {
    let program = r#"struct Person {
        age: i32,
    }

    impl Person {
        mt get_age() -> i32 {
            return self.age
        }

        mt change_age_to(new_age: i32) {
            self.age = new_age
        }
    }

    fn test() -> i32 {
        let mut p = Person { age: 123 }

        p.change_age_to(58)

        return p.get_age()
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignTwiceToImutable { .. })
    ));
}

#[test]
fn simple_enum() -> anyhow::Result<()> {
    let program = r#"enum Simple {
        A,
        B,
        C,
    }

    fn test() -> i32 {
        let b = Simple::B

        if b == Simple::B {
            return 58
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn simple_enum_as_argument() -> anyhow::Result<()> {
    let program = r#"enum Simple {
        A,
        B,
        C,
    }

    fn f(e: Simple) -> i32 {
        if e == Simple::A || e == Simple::B {
            return 123
        }
        return 58
    }

    fn test() -> i32 {
        return f(Simple::C)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tagged_enum() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn test() -> i32 {
        let e1 = E::A
        let e2 = E::B(Fruits { apple: 48, ichigo: 10 })
        return 58
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn use_tagged_enum() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn test() -> i32 {
        let e = E::B(Fruits { apple: 48, ichigo: 10 })

        return match e {
            E::A => 123,
            E::B => fruits.apple + fruits.ichigo,
        }
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tagged_enum_as_argument() -> anyhow::Result<()> {
    let program = r#"struct Fruits {
        apple: i32,
        ichigo: i32,
    }

    enum E {
        A,
        B(Fruits),
    }

    fn sum_fruits(e: E) -> i32 {
        return match e {
            E::A => 116,
            E::B => fruits.apple + fruits.ichigo,
        }
    }

    fn test() -> i32 {
        let e1 = E::A
        let e2 = E::B(Fruits { apple: 48, ichigo: 10 })
        return sum_fruits(e1) - sum_fruits(e2)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn simple_generics() -> anyhow::Result<()> {
    let program = r#"fn add_10<T>(n: T) -> T {
        return n + 10
    }

    fn test() -> i32 {
        return add_10<i32>(48)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn multiple_generic_parameters() -> anyhow::Result<()> {
    let program = r#"fn add<T, U, R>(n1: T, n2: U) -> R {
        return n1 + n2
    }

    fn test() -> i32 {
        return add<i32, i32, i32>(48, 10)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn struct_as_generic_argument() -> anyhow::Result<()> {
    let program = r#"struct Sample {
        n: i32,
    }

    fn get_n<S>(o: S) -> i32 {
        return o.n
    }

    fn test() -> i32 {
        let s = Sample { n: 58 }
        return get_n<Sample>(s)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn tuple_as_generic_argument() -> anyhow::Result<()> {
    let program = r#"fn get_third<T>(t: T) -> i32 {
        return t.2
    }

    fn test() -> i32 {
        let tup = (48, 10, 58)
        return get_third<(i32, i32, i32)>(tup)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn array_as_generic_argument() -> anyhow::Result<()> {
    let program = r#"fn get_third<T>(a: T) -> i32 {
        return a[2]
    }

    fn test() -> i32 {
        let a = [48, 10, 58]
        return get_third<[i32; 3]>(a)
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}
