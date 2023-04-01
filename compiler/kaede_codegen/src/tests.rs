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

    unsafe { ee.get_function::<TestFunc>("test.test").unwrap().call() }
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
fn shared_borrow() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let s = 58
        let r = &s
        return *r
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn reference_type_argument() -> anyhow::Result<()> {
    let program = r"fn dref(r: &i32) -> i32 {
        return *r
    }

    fn test() -> i32 {
        let n = 58
        return dref(&n)
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn mutable_borrow_and_deref() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
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
    let program = r"fn to_58(n: &mut i32) {
        *n = 58
    }

    fn test() -> i32 {
        let mut a = 123

        to_58(&mut a)

        return a
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_immutable_reference() {
    let program = r"fn test() -> i32 {
        let mut n = 58

        let r = &n;

        *r = 123

        return n
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignTwiceToImutable { .. })
    ));
}

#[test]
fn borrow_temporary_value() -> anyhow::Result<()> {
    let program = r"fn test() -> i32 {
        let r = &(58 / 2)

        let mr = &mut 29
        *mr = *mr + *r

        return *mr
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn mutable_references_to_immutable_variables() {
    let program = r"fn test() -> i32 {
        let n = 123
        let r = &mut n;
        return *r
    }";

    assert!(matches!(
        run_test(program),
        Err(CodegenError::MutableBorrowingFromImmutable { .. })
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
    let program = r"fn add_copy(a: [i32; 2]) -> i32 {
        return a[0] + a[1]
    }

    fn add(a: &[i32; 2]) -> i32 {
        return (*a)[0] + (*a)[1]
    }

    fn test() -> i32 {
        let a = [48, 10]

        return add(&a) + add_copy(a)
    }";

    assert_eq!(run_test(program)?, 116);

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
fn reference_tuple_unpacking() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let tup = (48, true, "hello", 10)

        let (n1, f, _, n2) = &tup

        if *f {
            return *n1 + *n2
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    let program = r#"fn test() -> i32 {
        let (n1, f, _, n2) = &(48, true, "hello", 10)

        if *f {
            return *n1 + *n2
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

    fn tup2(a: &(i32, bool)) -> i32 {
        let (n, f) = a

        if *f {
            return *n
        }

        return 124
    }

    fn test() -> i32 {
        let tup = (58, true)

        return tup1(tup) + tup2(&tup)
    }";

    assert_eq!(run_test(program)?, 116);

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
        let tuptup = ((48, true), (10, true))

        return tuptup.0.0 + tuptup.1.0
    }";

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn reference_tuple_indexing() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let tup = &(58, true, "hello")

        let t0 = tup.0

        if *tup.1 {
            return *t0
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn field_access_to_reference_struct() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn test() -> i32 {
        let person = &Person { is_male: false, stature: 48, age: 10, is_female: true }
        let age = person.age
        return *age + *person.stature
    }"#;

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
fn assign_to_reference_struct_field() -> anyhow::Result<()> {
    let program = r#"struct Person {
        age: i32,
        stature: i32,
        is_male: bool,
        is_female: bool,
    }

    fn test() -> i32 {
        let mut person = &mut Person {
            is_male: false,
            stature: 48,
            age: 0,
            is_female: true,
        }

        *person.age = 10
        return *person.age + *person.stature
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_reference_tuple_field() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let mut t = &mut (58, false)
        *t.1 = true

        if *t.1 {
            return *t.0
        }

        return 123
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_to_unpacked_reference_tuple_field() -> anyhow::Result<()> {
    let program = r#"fn test() -> i32 {
        let mut t = (123, true)

        let (n, _) = &mut t
        *n = 58

        if t.1 {
            return t.0
        }

        return 124
    }"#;

    assert_eq!(run_test(program)?, 58);

    Ok(())
}

#[test]
fn assign_immutable_ref_to_mutable_variable() {
    let program = r#"fn test() -> i32 {
        let n = 58
        let mut r = &n
        *r = 123
        return n
    }"#;

    assert!(matches!(
        run_test(program),
        Err(CodegenError::CannotAssignImmutableReferencesToMut { .. })
    ));
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
