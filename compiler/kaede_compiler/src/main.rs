use std::{fs, path::PathBuf};

use anyhow::anyhow;
use clap::Parser;
use inkwell::context::Context;
use kaede_codegen::{codegen, CodegenContext};
use kaede_lex::lex;
use kaede_parse::parse;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(value_name = "FILE")]
    files: Vec<PathBuf>,

    #[arg(
        short,
        long,
        help = "Instead of a file, we receive a program on the command line"
    )]
    program: Option<String>,
}

fn compile(file_path: PathBuf, program: &str) -> anyhow::Result<()> {
    let ast = parse(lex(program))?;

    let module_name = file_path.file_stem().unwrap().to_str().unwrap();

    let context = Context::create();
    let module = context.create_module(module_name);

    let cgcx = CodegenContext::new(&context)?;
    codegen(&cgcx, &module, file_path, ast)?;

    println!("{}", module.to_string());

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let files = args.files;

    if let Some(prog) = args.program.as_deref() {
        compile(PathBuf::from("commandline"), prog)?;
        return Ok(());
    }

    if files.is_empty() {
        return Err(anyhow!("No input files"));
    }

    if 1 < files.len() {
        return Err(anyhow!("Multiple input filenames provided"));
    }

    for file in files {
        let prog = fs::read_to_string(&file)?;
        compile(file, &prog)?;
    }

    Ok(())
}
