use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

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
