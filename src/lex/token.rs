#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Integer(u64),
    Eof,
}
