#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum Top {
    Function(Function),
}

pub type TranslationUnit = Vec<Top>;
