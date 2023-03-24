use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

static FILE_EXTENSION: &str = "kae";

fn create_kaede_file_name(stem: &str) -> String {
    format!("{}.{}", stem, FILE_EXTENSION)
}

#[test]
fn failed_to_open_file() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;

    cmd.arg("test/file/doesnt/exist");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Failed to open file"));

    Ok(())
}

#[test]
fn no_input_files() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("No input files"));

    Ok(())
}

/// Testing under the assumption that `lli` is installed.
#[test]
fn import_modules() -> anyhow::Result<()> {
    let tmp_dir = assert_fs::TempDir::new()?;

    let m1 = tmp_dir.child(create_kaede_file_name("m1"));
    m1.write_str("pub fn yoha() i32 { return 48 }")?;

    let m2 = tmp_dir.child(create_kaede_file_name("m2"));
    m2.write_str("pub fn io() i32 { return 10 }")?;

    let main = tmp_dir.child(create_kaede_file_name("main"));
    main.write_str("import m1\nimport m2\nfn main() i32 { return m1.yoha() + m2.io() }")?;

    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;
    cmd.args([main.path(), m1.path(), m2.path()]);
    let success = cmd.assert().success();

    let llvm_ir = assert_fs::NamedTempFile::new("ir")?;
    llvm_ir.write_binary(&success.get_output().stdout)?;

    Command::new("lli")
        .arg(llvm_ir.path())
        .assert()
        .code(predicate::eq(58));

    tmp_dir.close()?;

    Ok(())
}
