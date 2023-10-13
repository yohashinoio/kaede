use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn failed_to_open_file() -> anyhow::Result<()> {
    Command::cargo_bin(env!("CARGO_PKG_NAME"))?
        .arg("test/file/doesnt/exist")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Failed to open file"));

    Ok(())
}

#[test]
fn no_input_files() -> anyhow::Result<()> {
    Command::cargo_bin(env!("CARGO_PKG_NAME"))?
        .assert()
        .failure()
        .stderr(predicate::str::contains("No input files"));

    Ok(())
}
