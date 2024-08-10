use kaede_span::{file::FilePath, Span};

use crate::token::{Token, TokenKind};

/// The rules are based on the Go language, thank you very much!
///
/// `Newline` token will be removed
pub fn insert_semi(tokens: impl Iterator<Item = Token>, file: FilePath) -> Vec<Token> {
    let mut result = Vec::<Token>::new();

    for tok in tokens {
        match tok.kind {
            TokenKind::NewLine | TokenKind::Eoi => {
                if let Some(last) = result.last() {
                    if is_semi_insertable(&last.kind) {
                        let start = last.span.finish;
                        let mut finish = start;
                        finish.increase_column();

                        result.push(Token {
                            kind: TokenKind::Semi,
                            span: Span::new(start, finish, file),
                        });
                    }
                }

                // Eoi not discarded
                if tok.kind == TokenKind::Eoi {
                    result.push(tok);
                }
            }

            _ => result.push(tok),
        }
    }

    result
}

fn is_semi_insertable(token: &TokenKind) -> bool {
    use TokenKind::*;

    matches!(
        token,
        Int(_)
            | Ident(_)
            | StringLiteral(_)
            | CloseParen
            | CloseBrace
            | CloseBracket
            | Return
            | True
            | False
    )
}
