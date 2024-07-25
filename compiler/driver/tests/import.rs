//! Testing under the assumption that `lli` is installed!

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

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

    let compile_output = Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            "--display-llvm-ir",
            main.path().to_str().unwrap(),
            module1.path().to_str().unwrap(),
            module2.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let llvm_ir = assert_fs::NamedTempFile::new("ir")?;
    llvm_ir.write_binary(&compile_output.get_output().stdout)?;

    Command::new("lli")
        .arg(llvm_ir.path())
        .assert()
        .code(predicate::eq(58));

    Ok(())
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

    let compile_output = Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            "--display-llvm-ir",
            main.path().to_str().unwrap(),
            module.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let llvm_ir = assert_fs::NamedTempFile::new("ir")?;
    llvm_ir.write_binary(&compile_output.get_output().stdout)?;

    Command::new("lli")
        .arg(llvm_ir.path())
        .assert()
        .code(predicate::eq(58));

    Ok(())
}

#[test]
fn import_struct_methods() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child("m1.kd");
    module1.write_str(
        r#"struct Apple {
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
        r#"struct Ichigo {
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

    let compile_output = Command::cargo_bin(env!("CARGO_BIN_EXE_kaede"))?
        .args([
            "--display-llvm-ir",
            main.path().to_str().unwrap(),
            module1.path().to_str().unwrap(),
            module2.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let llvm_ir = assert_fs::NamedTempFile::new("ir")?;
    llvm_ir.write_binary(&compile_output.get_output().stdout)?;

    Command::new("lli")
        .arg(llvm_ir.path())
        .assert()
        .code(predicate::eq(58));

    Ok(())
}
