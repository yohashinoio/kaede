#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Integer(u64),
    Ident(String),

    // Punctuators
    /// '('
    OpenParen,
    /// ')'
    CloseParen,
    /// '{'
    OpenBrace,
    /// '}'
    CloseBrace,
    /// ','
    Comma,

    // Operators
    Add,
    Sub,
    Mul,
    Div,

    // Reserved words
    Function,

    /// End of file
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenKind::*;

        match self {
            Integer(_) => write!(f, "integer"),
            Ident(_) => write!(f, "identifier"),

            OpenParen => write!(f, "'('"),
            CloseParen => write!(f, "')'"),
            OpenBrace => write!(f, "'{{'"),
            CloseBrace => write!(f, "'}}'"),
            Comma => write!(f, "','"),

            Add => write!(f, "'+'"),
            Sub => write!(f, "'-'"),
            Mul => write!(f, "'*'"),
            Div => write!(f, "'/'"),

            Function => write!(f, "function"),

            Eof => write!(f, "EOF"),
        }
    }
}
