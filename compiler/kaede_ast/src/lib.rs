use top::TopLevel;

pub mod expr;
pub mod stmt;
pub mod top;

pub struct CompileUnit {
    pub top_levels: Vec<TopLevel>,
}
