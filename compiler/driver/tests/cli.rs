use assert_cmd::{crate_name, prelude::*};
use predicates::prelude::*;
use std::process::Command;

#[test]
fn failed_to_open_file() -> anyhow::Result<()> {
    Command::cargo_bin(crate_name!())?
        .arg("test/file/doesnt/exist")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Failed to open file"));

    Ok(())
}

#[test]
fn no_input_files() -> anyhow::Result<()> {
    Command::cargo_bin(crate_name!())?
        .assert()
        .failure()
        .stderr(predicate::str::contains("No input files"));

    Ok(())
}
