use std::{fs, path::PathBuf, vec};

use anyhow::{anyhow, Context as _};
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

struct CompileUnitInfo {
    pub file_path: PathBuf,
    pub program: String,
}

fn compile(unit_infos: Vec<CompileUnitInfo>) -> anyhow::Result<()> {
    let context = Context::create();

    let mut compiled_modules = Vec::new();

    for unit_info in unit_infos {
        let file_path = unit_info.file_path;

        let ast = parse(lex(&unit_info.program))?;

        let module_name = file_path.file_stem().unwrap().to_str().unwrap();

        let module = context.create_module(module_name);
        module.set_source_file_name(file_path.to_str().unwrap());

        let cgcx = CodegenContext::new(&context)?;
        codegen(&cgcx, &module, file_path, ast)?;

        compiled_modules.push(module);
    }

    let module = compiled_modules.pop().unwrap();

    // Link modules
    for other_module in compiled_modules {
        module
            .link_in_module(other_module)
            .map_err(|e| anyhow!(e.to_string()))?;
    }

    println!("{}", module.to_string());

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let file_paths = args.files;

    if let Some(program) = args.program {
        compile(vec![CompileUnitInfo {
            file_path: PathBuf::from("commandline"),
            program,
        }])?;

        return Ok(());
    }

    if file_paths.is_empty() {
        return Err(anyhow!("No input files"));
    }

    // --- Compile ---

    let mut programs = Vec::new();

    for file_path in file_paths {
        programs.push(CompileUnitInfo {
            program: fs::read_to_string(&file_path)
                .with_context(|| format!("Failed to open file: {}", file_path.to_string_lossy()))?,
            file_path,
        });
    }

    compile(programs)?;

    Ok(())
}
