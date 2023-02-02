use inkwell::context::Context;
use kaede_codegen::codegen;
use kaede_lex::lex;
use kaede_parse::parse;

fn main() -> anyhow::Result<()> {
    let tokens = lex(r" fn f() { -((+48 +10) * -2)}");

    let ast = parse(tokens)?;

    println!("{:?}", ast);

    let context = Context::create();
    let module = context.create_module("sample");
    codegen(&context, &module, &ast);

    println!("{}", module.to_string());

    Ok(())
}
