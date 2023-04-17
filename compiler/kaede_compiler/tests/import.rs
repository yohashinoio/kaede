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
    let tmp_dir = assert_fs::TempDir::new()?;

    let mod_1 = tmp_dir.child(create_kaede_file_name("m1"));
    mod_1.write_str("pub fn yoha() -> i32 { return 48 }")?;

    let mod_2 = tmp_dir.child(create_kaede_file_name("m2"));
    mod_2.write_str("pub fn io() -> i32 { return 10 }")?;

    let main = tmp_dir.child(create_kaede_file_name("main"));
    main.write_str("import m1\nimport m2\nfn main() -> i32 { return m1.yoha() + m2.io() }")?;

    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;
    cmd.args([main.path(), mod_1.path(), mod_2.path()]);
    let success = cmd.assert().success();

    let ir = assert_fs::NamedTempFile::new("ir")?;
    ir.write_binary(&success.get_output().stdout)?;

    Command::new("lli")
        .arg(ir.path())
        .assert()
        .code(predicate::eq(58));

    tmp_dir.close()?;

    Ok(())
}
