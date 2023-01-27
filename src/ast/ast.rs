#[derive(Debug, PartialEq)]
pub struct Add {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Sub {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Mul {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Div {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Integer(u64),

    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Expr,
}

#[derive(Debug, PartialEq)]
pub enum Top {
    Function(Function),
}

pub type TranslationUnit = Vec<Top>;
