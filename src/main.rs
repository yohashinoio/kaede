use inkwell::context::Context;

use crate::{codegen::codegen::CodeGen, lex::lex, parse::parse};

mod ast;
mod codegen;
mod lex;
mod parse;

fn main() -> anyhow::Result<()> {
    let tokens = lex(" fn f() {}");

    let ast = parse(tokens)?;

    println!("{:?}", ast);

    let context = Context::create();
    let gen = CodeGen::new(&context, "sample");
    let module = gen.codegen_module(&ast);

    println!("{}", module.print_to_string().to_string());

    Ok(())
}
