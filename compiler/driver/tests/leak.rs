use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

const KAEDE_GC_LIB_PATH: &str = concat!(env!("HOME"), "/.kaede/lib/libkgc.so");

#[test]
fn leak_check_with_valgrind() -> anyhow::Result<()> {
    let program_file = assert_fs::NamedTempFile::new("leak.kd")?;
    program_file.write_str(
        r"struct Num {
            n: i32,
        }

        fn f(): ([i32; 3], (i32, i32, Num)) {
            let a = [1, 2, 3]
            let t = (48, 10, Num { n: 58 })
            return (a, t)
        }

        fn main(): i32 {
            let mut c = 0

            loop {
                let num = Num { n: 58 }

                if c == 1000 {
                    return num.n
                }

                c = c + 1
            }

            return 123
        }",
    )?;

    // Compile
    let compile_output = Command::cargo_bin(env!("CARGO_PKG_NAME"))?
        .args([
            "-O0",
            "--display-llvm-ir",
            program_file.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let llvm_ir = assert_fs::NamedTempFile::new("leak.ll")?;
    llvm_ir.write_binary(&compile_output.get_output().stdout)?;

    let asm = assert_fs::NamedTempFile::new("leak.s")?;
    asm.write_binary(
        &Command::new("llc")
            .args([llvm_ir.path().to_str().unwrap(), "-o", "-"])
            .assert()
            .success()
            .get_output()
            .stdout,
    )?;

    let executable = assert_fs::NamedTempFile::new("leak")?;
    Command::new("cc")
        .args([
            "-g",
            asm.path().to_str().unwrap(),
            KAEDE_GC_LIB_PATH,
            "-o",
            executable.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    // Leak check
    Command::new("valgrind")
        .args(["--leak-check=full", executable.path().to_str().unwrap()])
        .assert()
        .code(predicate::eq(58))
        .stderr(
            // Valgrind results display changes depending on version
            predicate::str::contains("definitely lost: 0")
                .or(predicate::str::contains("definitely lost").count(0)),
        );

    Ok(())
}
