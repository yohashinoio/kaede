use top::TopLevel;

pub mod expr;
pub mod stmt;
pub mod top;

#[derive(Debug, PartialEq, Eq)]
pub struct CompileUnit {
    pub top_levels: Vec<TopLevel>,
}
