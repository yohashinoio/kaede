use core::panic;
use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    vec,
};

use anyhow::{anyhow, Context as _};
use colored::Colorize;
use inkwell::{context::Context, module::Module, OptimizationLevel};
use kaede_codegen::{codegen_compile_unit, error::CodegenError, CodegenCtx};
use kaede_common::kaede_dir;
use kaede_parse::Parser;
use tempfile::{NamedTempFile, TempPath};

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long, action)]
    display_llvm_ir: bool,

    #[arg(value_name = "FILE")]
    files: Vec<PathBuf>,

    #[arg(
        short,
        long,
        help = "instead of a file, we receive a program from the commandline argument"
    )]
    program: Option<String>,

    #[arg(short = 'o')]
    output: Option<PathBuf>,

    #[arg(short = 'O', default_value_t = 2, help = "optimization level (0-3)")]
    opt_level: u8,

    #[arg(
        short = 'c',
        action,
        help = "Outputs object files without invoking the linker"
    )]
    c: bool,

    // Do not load standard libraries that automatically load
    // Will not be used except when building standard libraries
    #[arg(long, action)]
    no_autoload: bool,
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

fn emit_bitcode_to_tempfile(module: &Module) -> anyhow::Result<TempPath> {
    let tempfile = NamedTempFile::new()?;

    let temppath = tempfile.into_temp_path();

    module.write_bitcode_to_path(&temppath);

    Ok(temppath)
}

fn emit_object_file_to_tempfile(ir_path: &Path) -> anyhow::Result<TempPath> {
    let tempfile = NamedTempFile::new()?;

    let temppath = tempfile.into_temp_path();

    let status = Command::new("llc")
        .args([
            "-filetype=obj",
            "-relocation-model=pic", // Set relocation model
            "-o",
            temppath.to_str().unwrap(),
            ir_path.to_str().unwrap(),
        ])
        .status()?;

    if !status.success() {
        anyhow::bail!("Failed to emit object file using 'llc'")
    }

    Ok(temppath)
}

fn emit_exe_file(obj_path: &Path, output_file_path: &Path) -> anyhow::Result<()> {
    let status = Command::new("cc")
        .args([
            "-o",
            output_file_path.to_str().unwrap(),
            obj_path.to_str().unwrap(),
            &format!("{}/lib/libkgc.so", kaede_dir()), // Link with garbage collector
            &format!("{}/lib/libkd.so", kaede_dir()),  // Link with standard library
        ])
        .status()?;

    if !status.success() {
        anyhow::bail!("Failed to emit executable file using 'cc'")
    }

    Ok(())
}

fn compile<'ctx>(
    cgcx: &'ctx CodegenCtx<'_>,
    unit_infos: Vec<CompileUnitInfo>,
    opt_level: OptimizationLevel,
    no_autoload: bool,
) -> anyhow::Result<Module<'ctx>> {
    let mut compiled_modules = Vec::new();

    for unit_info in unit_infos {
        let ast = Parser::new(&unit_info.program).run()?;

        let module = codegen_compile_unit(cgcx, unit_info.file_path, ast, opt_level, no_autoload)?;

        compiled_modules.push(module);
    }

    let module = compiled_modules.pop().unwrap();

    // Link modules
    for other_module in compiled_modules {
        module
            .link_in_module(other_module)
            .map_err(|e| anyhow!(e.to_string()))?;
    }

    Ok(module)
}

fn compile_and_output_obj(
    unit_infos: Vec<CompileUnitInfo>,
    opt_level: OptimizationLevel,
    display_llvm_ir: bool,
    output_file_path: &Path,
    no_autoload: bool,
) -> anyhow::Result<()> {
    let context = Context::create();
    let cgcx = CodegenCtx::new(&context)?;

    let module = compile(&cgcx, unit_infos, opt_level, no_autoload)?;

    // Emit
    if display_llvm_ir {
        // Print llvm ir to stdout
        print!("{}", module.to_string());
    } else {
        // Emit executable file
        let ir_path = emit_bitcode_to_tempfile(&module)?;

        let obj_path = emit_object_file_to_tempfile(&ir_path)?;

        fs::rename(obj_path, output_file_path)?;
    }

    Ok(())
}

fn compile_and_link(
    unit_infos: Vec<CompileUnitInfo>,
    opt_level: OptimizationLevel,
    display_llvm_ir: bool,
    output_file_path: &Path,
    no_autoload: bool,
) -> anyhow::Result<()> {
    let context = Context::create();
    let cgcx = CodegenCtx::new(&context)?;

    let module = compile(&cgcx, unit_infos, opt_level, no_autoload)?;

    if module.get_function("main").is_none() {
        return Err(CodegenError::MainNotFound.into());
    }

    // Emit
    if display_llvm_ir {
        // Print llvm ir to stdout
        print!("{}", module.to_string());
    } else {
        // Emit executable file
        let ir_path = emit_bitcode_to_tempfile(&module)?;

        let obj_path = emit_object_file_to_tempfile(&ir_path)?;

        emit_exe_file(&obj_path, output_file_path)?;
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    use clap::Parser;

    let args = Args::parse();

    let file_paths = args.files;

    let opt_level = to_inkwell_opt_level(args.opt_level);

    let display_llvmir = args.display_llvm_ir;

    let output_file_path =
        &args
            .output
            .unwrap_or(PathBuf::from(if args.c { "a.o" } else { "a.out" }));

    if let Some(program) = args.program {
        compile_and_link(
            vec![CompileUnitInfo {
                file_path: PathBuf::from("<commandline>"),
                program,
            }],
            opt_level,
            display_llvmir,
            output_file_path,
            args.no_autoload,
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

    if args.c {
        // Emit object files
        if let Err(err) = compile_and_output_obj(
            programs,
            opt_level,
            display_llvmir,
            output_file_path,
            args.no_autoload,
        ) {
            // If backtrace is enabled, it is also displayed
            eprintln!("{}: {:?}", "Error".bright_red(), err);
        }
    } else {
        // Emit exe files
        if let Err(err) = compile_and_link(
            programs,
            opt_level,
            display_llvmir,
            output_file_path,
            args.no_autoload,
        ) {
            // If backtrace is enabled, it is also displayed
            eprintln!("{}: {:?}", "Error".bright_red(), err);
        }
    }

    Ok(())
}
