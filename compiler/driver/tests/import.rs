//! Testing under the assumption that `lli` is installed!

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::{ffi::OsString, path::Path, process::Command};

fn test(expect: i32, file_paths: &[&Path]) -> anyhow::Result<()> {
    let exe = assert_fs::NamedTempFile::new("a.out")?;

    let mut args = file_paths
        .into_iter()
        .map(|p| p.as_os_str().to_os_string())
        .collect::<Vec<OsString>>();

    args.push(OsString::from("-o"));
    args.push(exe.path().as_os_str().to_os_string());

    let compile_output = Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args(args)
        .assert()
        .success();

    let llvm_ir = assert_fs::NamedTempFile::new("ir")?;
    llvm_ir.write_binary(&compile_output.get_output().stdout)?;

    Command::new(format!("{}", exe.path().to_str().unwrap()))
        .assert()
        .code(predicate::eq(expect));

    Ok(())
}

#[test]
fn import_functions() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str("pub fn yoha(): i32 { return 48 }")?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str("pub fn io(): i32 { return 10 }")?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m1
        import m2
        fn main(): i32 {
            return m1.yoha() + m2.io()
        }"#,
    )?;

    test(58, &[module1.path(), module2.path(), main.path()])
}

#[test]
fn import_i32_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"impl i32 {
            pub fn add(self, other: i32): i32 {
                return self + other
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            return 48.add(10)
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn import_struct_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"pub struct Apple {
            size: i32,
        }
        impl Apple {
            pub fn new(size: i32): Apple {
                return Apple { size: size }
            }

            pub fn is_orange(self): bool {
                return false
            }
        }"#,
    )?;

    let module2 = tempdir.child("m2.kd");
    module2.write_str(
        r#"pub struct Ichigo {
            size: i32,
        }
        impl Ichigo {
            pub fn get_size(self): i32 {
                return self.size
            }
        }
        "#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m1
        import m2
        fn main(): i32 {
            let apple = m1.Apple::new(48);
            let ichigo = m2.Ichigo { size: 10 }
            if !apple.is_orange() {
                return apple.size + ichigo.get_size()
            }
            return 123
        }"#,
    )?;

    test(58, &[module1.path(), module2.path(), main.path()])
}

#[test]
fn import_struct_methods_with_name_conflict() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m1.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m1
        struct Apple {
            size: i32,
        }
        fn main(): i32 {
            let apple1 = m1.Apple { size: 48 };
            let apple2 = Apple { size: 10 }
            return apple1.size + apple2.size
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn imported_typed_member() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        struct Fruit {
            apple: m.Apple,
        }
        fn main(): i32 {
            let fruit = Fruit { apple: m.Apple { size: 58 } }
            return fruit.apple.size
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn import_enum() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub enum Fruit {
            Apple(i32),
            Orange,
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::Apple(58);

            match apple {
                m.Fruit::Apple(value) => {
                    return value
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn import_complex_enum() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        pub enum Fruit {
            Apple(Apple),
            Ichigo(i32),
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::Apple(m.Apple { size: 48 });
            let ichigo = m.Fruit::Ichigo(10);

            match apple {
                m.Fruit::Apple(a) => {
                    match ichigo {
                        m.Fruit::Ichigo(i) => {
                            return a.size + i
                        },
                        _ => return 123,
                    }
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn enum_with_imported_struct() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }
        impl Apple {
            pub fn get_size(self): i32 {
                return self.size
            }
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        enum Fruit {
            Apple(m.Apple),
            Ichigo(i32),
        }

        fn main(): i32 {
            let apple = Fruit::Apple(m.Apple { size: 48 });
            let ichigo = Fruit::Ichigo(10);

            match apple {
                Fruit::Apple(a) => {
                    match ichigo {
                        Fruit::Ichigo(i) => {
                            return a.get_size() + i
                        },
                        _ => return 123,
                    }
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn import_enum_and_call_variant_method() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module = tempdir.child("m.kd");
    module.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        impl Apple {
            pub fn get_size(self): i32 {
                return self.size
            }
        }

        pub enum Fruit {
            Apple(Apple),
            Ichigo(i32),
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m
        fn main(): i32 {
            let apple = m.Fruit::Apple(m.Apple { size: 48 });

            match apple {
                m.Fruit::Apple(a) => {
                    return a.get_size()
                },
                _ => return 123,
            }

            return 256
        }"#,
    )?;

    test(58, &[module.path(), main.path()])
}

#[test]
fn nested_import() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let m1 = tempdir.child("m1.kd");
    m1.write_str(
        r#"pub struct Apple {
            size: i32,
        }

        pub enum Fruit {
            Apple(Apple),
            Ichigo(i32),
        }"#,
    )?;

    let m2 = tempdir.child("m2.kd");
    m2.write_str(
        r#"import m1
        pub fn get_value(fruit: m1.Fruit): i32 {
            return match fruit {
                m1.Fruit::Apple(a) => return a.size,
                m1.Fruit::Ichigo(n) => return n,
            }
        }
        pub fn f(): i32 {
            let fruit = m1.Fruit::Apple(m1.Apple { size: 48 });
            return get_value(fruit)
        }
        pub fn g(): i32 {
            let fruit = m1.Fruit::Ichigo(10);
            return get_value(fruit)
        }"#,
    )?;

    let main = tempdir.child("main.kd");
    main.write_str(
        r#"import m2
        fn main(): i32 {
            return m2.f() + m2.g()
        }"#,
    )?;

    test(58, &[m1.path(), m2.path(), main.path()])
}
