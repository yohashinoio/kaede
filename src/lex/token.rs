#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Integer(u64),
    Ident(String),

    // Punctuators
    OpenParen,  // '('
    CloseParen, // ')'
    OpenBrace,  // '{'
    CloseBrace, // '}'
    Comma,      // ','

    // Reserved words
    Function, // "fn"

    Eof,
}
