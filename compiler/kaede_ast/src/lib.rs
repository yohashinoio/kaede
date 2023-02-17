use kaede_type::TypeEnum;

#[derive(Debug, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncCall {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Integer(u64),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
    Ident(String),
    FuncCall(FuncCall),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Return(pub Option<Expr>);

#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    pub name: String,
    pub init: Option<Expr>,
    pub ty: TypeEnum,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Return(Return),
    Let(Let),
}

pub type StmtList = Vec<Stmt>;

#[derive(Debug, PartialEq, Eq)]
pub enum Top {
    Function { name: String, body: StmtList },
}

pub type TranslationUnit = Vec<Top>;
