//! Testing under the assumption that `lli` is installed!

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

static FILE_EXTENSION: &str = "kae";

fn create_kaede_file_name(stem: &str) -> String {
    format!("{}.{}", stem, FILE_EXTENSION)
}

#[test]
fn import_functions() -> anyhow::Result<()> {
    let tempdir = assert_fs::TempDir::new()?;

    let module1 = tempdir.child(create_kaede_file_name("m1"));
    module1.write_str("pub fn yoha() -> i32 { return 48 }")?;

    let module2 = tempdir.child(create_kaede_file_name("m2"));
    module2.write_str("pub fn io() -> i32 { return 10 }")?;

    let main = tempdir.child(create_kaede_file_name("main"));
    main.write_str("import m1\nimport m2\nfn main() -> i32 { return m1.yoha() + m2.io() }")?;

    let compile_output = Command::cargo_bin(env!("CARGO_PKG_NAME"))?
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
