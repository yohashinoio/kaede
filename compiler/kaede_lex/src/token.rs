use kaede_location::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Int(String),
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
    /// ';'
    Semi,

    // Operators
    /// '+'
    Add,
    /// '-'
    Sub,
    /// '*'
    Mul,
    /// '/'
    Div,
    /// '='
    Assign,
    /// "=="
    Eq,

    // Reserved words
    /// "fn"
    Function,
    /// "return"
    Return,
    /// "let"
    Let,
    /// "mut"
    Mut,
    /// "if"
    If,
    /// "else"
    Else,

    /// End of input
    Eoi,

    NewLine,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenKind::*;

        match self {
            Int(_) => write!(f, "integer"),
            Ident(_) => write!(f, "identifier"),

            OpenParen => write!(f, "'('"),
            CloseParen => write!(f, "')'"),
            OpenBrace => write!(f, "'{{'"),
            CloseBrace => write!(f, "'}}'"),
            Comma => write!(f, "','"),
            Semi => write!(f, "';'"),

            Add => write!(f, "'+'"),
            Sub => write!(f, "'-'"),
            Mul => write!(f, "'*'"),
            Div => write!(f, "'/'"),
            Assign => write!(f, "'='"),
            Eq => write!(f, "'=='"),

            Function => write!(f, "'fn'"),
            Return => write!(f, "'return'"),
            Let => write!(f, "'let'"),
            Mut => write!(f, "'mut'"),
            If => write!(f, "'if'"),
            Else => write!(f, "'else'"),

            Eoi => write!(f, "end of input"),

            NewLine => write!(f, "newline"),
        }
    }
}
