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
pub struct Return(pub Option<Expr>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Return(Return),
    Let(Let),
}

pub type StmtList = Vec<Stmt>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Top {
    Function { name: String, body: StmtList },
}

pub type TranslationUnit = Vec<Top>;
