use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn leak_check_with_valgrind() -> anyhow::Result<()> {
    let tmp_dir = assert_fs::TempDir::new()?;

    let file = tmp_dir.child("memalloc");
    file.write_str(
        r"struct A {
            n: i32,
        }

        fn main() -> i32 {
            let mut count = 0

            loop {
                let a = A { n: 58 }

                if count == 1000 {
                    return a.n
                }

                count = count + 1
            }

            return 123
        }",
    )?;

    // Compile
    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;
    cmd.args(["-O0", "--display-llvm-ir", file.path().to_str().unwrap()]);
    let success = cmd.assert().success();

    let ir = assert_fs::NamedTempFile::new("ir.ll")?;
    ir.write_binary(&success.get_output().stdout)?;

    let asm = assert_fs::NamedTempFile::new("asm.s")?;
    asm.write_binary(
        &Command::new("llc")
            .args([ir.path().to_str().unwrap(), "-o", "-"])
            .assert()
            .success()
            .get_output()
            .stdout,
    )?;

    let exe = assert_fs::NamedTempFile::new("exe")?;
    Command::new("cc")
        .args([
            "-g",
            asm.path().to_str().unwrap(),
            "-lgc",
            "-o",
            exe.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    // Leak check
    Command::new("valgrind")
        .args(["--leak-check=full", exe.path().to_str().unwrap()])
        .assert()
        .code(predicate::eq(58))
        .stderr(
            // Valgrind results display changes depending on version
            predicate::str::contains("definitely lost: 0")
                .or(predicate::str::contains("definitely lost").count(0)),
        );

    tmp_dir.close()?;

    Ok(())
}
