use kaede_span::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Int(String),
    Ident(String),
    StringLiteral(String),

    // Punctuators
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// ","
    Comma,
    /// ";"
    Semi,
    /// "."
    Dot,

    // Operators
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Asterisk,
    /// "/"
    Slash,
    /// "="
    Eq,
    /// "=="
    DoubleEq,
    /// "&"
    And,

    // Reserved words
    /// "fn"
    Fn,
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
    /// "loop"
    Loop,
    /// "break"
    Break,
    /// "struct"
    Struct,

    /// "true"
    True,
    /// "false"
    False,

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
            StringLiteral(_) => write!(f, "string literal"),

            OpenParen => write!(f, "'('"),
            CloseParen => write!(f, "')'"),
            OpenBrace => write!(f, "'{{'"),
            CloseBrace => write!(f, "'}}'"),
            Comma => write!(f, "','"),
            Semi => write!(f, "';'"),
            Dot => write!(f, "'.'"),

            Plus => write!(f, "'+'"),
            Minus => write!(f, "'-'"),
            Asterisk => write!(f, "'*'"),
            Slash => write!(f, "'/'"),
            Eq => write!(f, "'='"),
            DoubleEq => write!(f, "'=='"),
            And => write!(f, "'&'"),

            Fn => write!(f, "'fn'"),
            Return => write!(f, "'return'"),
            Let => write!(f, "'let'"),
            Mut => write!(f, "'mut'"),
            If => write!(f, "'if'"),
            Else => write!(f, "'else'"),
            Loop => write!(f, "'loop'"),
            Break => write!(f, "'break'"),
            Struct => write!(f, "'struct'"),

            True => write!(f, "'true'"),
            False => write!(f, "'false'"),

            Eoi => write!(f, "end of input"),

            NewLine => write!(f, "newline"),
        }
    }
}
