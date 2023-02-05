#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Integer(u64),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Top {
    Function { name: String, body: Option<Expr> },
}

pub type TranslationUnit = Vec<Top>;
