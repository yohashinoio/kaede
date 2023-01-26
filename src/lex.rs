// Created with reference to rustc_lexer. Thank you very much.

mod cursor;
mod lex;
pub mod token;

#[cfg(test)]
mod tests;

pub use lex::lex;
