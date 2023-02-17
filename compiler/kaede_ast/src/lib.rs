use stmt::StmtList;

pub mod expr;
pub mod stmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Top {
    Function { name: String, body: StmtList },
}

pub type TranslationUnit = Vec<Top>;
