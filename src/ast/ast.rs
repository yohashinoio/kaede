#[derive(Debug, PartialEq)]
pub enum Top {
    Function { name: String },
}

pub type TranslationUnit = Vec<Top>;
