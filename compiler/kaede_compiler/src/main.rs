use core::panic;
use std::{fs, path::PathBuf, vec};

use anyhow::{anyhow, Context as _};
use clap::Parser;
use inkwell::{context::Context, OptimizationLevel};
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

    /// Optimization level
    ///
    /// 0 `None`
    ///
    /// 1 `Less`
    ///
    /// 2 `Default`
    ///
    /// 3 `Aggressive`
    #[arg(short = 'O', default_value_t = 2)]
    opt_level: u8,
}

fn to_inkwell_opt_level(level: u8) -> OptimizationLevel {
    match level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,
        _ => panic!("Optimization levels range from 0 to 3!"),
    }
}

struct CompileUnitInfo {
    pub file_path: PathBuf,
    pub program: String,
}

fn compile(unit_infos: Vec<CompileUnitInfo>, opt_level: OptimizationLevel) -> anyhow::Result<()> {
    let context = Context::create();

    let mut compiled_modules = Vec::new();

    for unit_info in unit_infos {
        let file_path = unit_info.file_path;

        let ast = parse(lex(&unit_info.program))?;

        let module_name = file_path.file_stem().unwrap().to_str().unwrap();

        let module = context.create_module(module_name);
        module.set_source_file_name(file_path.to_str().unwrap());

        let cgcx = CodegenContext::new(&context)?;
        codegen(&cgcx, &module, file_path, ast, opt_level)?;

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

    let opt_level = to_inkwell_opt_level(args.opt_level);

    if let Some(program) = args.program {
        compile(
            vec![CompileUnitInfo {
                file_path: PathBuf::from("<commandline>"),
                program,
            }],
            opt_level,
        )?;

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

    compile(programs, opt_level)?;

    Ok(())
}
