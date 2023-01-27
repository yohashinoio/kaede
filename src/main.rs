use inkwell::context::Context;

use crate::{codegen::codegen, lex::lex, parse::parse};

mod ast;
mod codegen;
mod lex;
mod parse;

fn main() -> anyhow::Result<()> {
    let tokens = lex(r" fn f() { 48 +10 * 2}");

    let ast = parse(tokens)?;

    println!("{:?}", ast);

    let context = Context::create();
    let module = context.create_module("sample");
    codegen(&context, &module, &ast);

    println!("{}", module.to_string());

    Ok(())
}
