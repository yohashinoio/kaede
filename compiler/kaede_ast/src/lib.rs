use kaede_location::Spanned;
use stmt::StmtList;

pub mod expr;
pub mod stmt;

#[derive(Debug, PartialEq, Eq)]
pub enum TopEnum {
    Function { name: String, body: StmtList },
}

pub type Top = Spanned<TopEnum>;

pub type TranslationUnit = Vec<Top>;
