use cursor::Cursor;
use kaede_location::Span;
use token::{Token, TokenKind};

mod cursor;
pub mod token;

#[cfg(test)]
mod tests;

pub fn lex(input: &str) -> impl Iterator<Item = Token> + '_ {
    insert_semicolons(lex_internal(input)).into_iter()
}

fn lex_internal(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);

    let mut result = Vec::new();

    loop {
        let token = cursor.advance_token();

        match token.kind {
            TokenKind::Eoi => {
                result.push(token);
                break;
            }

            _ => result.push(token),
        }
    }

    result.into_iter()
}

/// Rules similar to Go language
/// 'NewLine' token will be removed
fn insert_semicolons(tokens: impl Iterator<Item = Token>) -> Vec<Token> {
    let mut result = Vec::<Token>::new();

    for tok in tokens {
        match tok.kind {
            TokenKind::NewLine | TokenKind::Eoi => {
                if let Some(last) = result.last() {
                    if can_insert_semicolon(&last.kind) {
                        let start = last.span.finish;
                        let mut finish = start;
                        finish.increase_column();

                        result.push(Token {
                            kind: TokenKind::Semi,
                            span: Span::new(start, finish),
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

/// True if a semicolon can be inserted after the token
fn can_insert_semicolon(token: &TokenKind) -> bool {
    use TokenKind::*;

    matches!(
        token,
        Int(_) | Ident(_) | StringLiteral(_) | CloseParen | CloseBrace | Return | True | False
    )
}

fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a first character of an identifier
fn is_id_start(c: char) -> bool {
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier
fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

impl Cursor<'_> {
    /// Insert span.
    fn create_token(&self, t: TokenKind) -> Token {
        Token {
            kind: t,
            span: self.span_builder.build(),
        }
    }

    fn advance_token(&mut self) -> Token {
        self.span_builder.start();

        let first_char = match self.bump() {
            Some(c) => c,
            None => return self.create_token(TokenKind::Eoi),
        };

        match first_char {
            '\n' => self.create_token(TokenKind::NewLine),

            // Skipper
            c if is_whitespace(c) => {
                self.eat_whitespace();
                self.advance_token()
            }

            // Number
            c @ '0'..='9' => {
                let n = self.number(c);
                self.create_token(TokenKind::Int(n))
            }

            // Identifier or reserved words
            c if is_id_start(c) => {
                let ident = self.ident(c);

                match ident.as_str() {
                    // Reserved words
                    "fn" => self.create_token(TokenKind::Fn),
                    "return" => self.create_token(TokenKind::Return),
                    "let" => self.create_token(TokenKind::Let),
                    "if" => self.create_token(TokenKind::If),
                    "else" => self.create_token(TokenKind::Else),
                    "loop" => self.create_token(TokenKind::Loop),
                    "break" => self.create_token(TokenKind::Break),
                    "mut" => self.create_token(TokenKind::Mut),
                    "struct" => self.create_token(TokenKind::Struct),
                    "true" => self.create_token(TokenKind::True),
                    "false" => self.create_token(TokenKind::False),
                    _ => self.create_token(TokenKind::Ident(ident)),
                }
            }

            // String literal
            '"' => {
                let lit = self.string_literal();
                self.create_token(TokenKind::StringLiteral(lit))
            }

            // Punctuators
            '(' => self.create_token(TokenKind::OpenParen),
            ')' => self.create_token(TokenKind::CloseParen),
            '{' => self.create_token(TokenKind::OpenBrace),
            '}' => self.create_token(TokenKind::CloseBrace),
            ',' => self.create_token(TokenKind::Comma),
            ';' => self.create_token(TokenKind::Semi),
            '.' => self.create_token(TokenKind::Dot),
            '=' => {
                if self.first() == '=' {
                    // ==
                    self.bump().unwrap();
                    self.create_token(TokenKind::Eq)
                } else {
                    // =
                    self.create_token(TokenKind::Assign)
                }
            }

            // Operators
            '+' => self.create_token(TokenKind::Add),
            '-' => self.create_token(TokenKind::Sub),
            '*' => self.create_token(TokenKind::Mul),
            '/' => self.create_token(TokenKind::Div),

            c => unreachable!("{}", c),
        }
    }

    fn eat_whitespace(&mut self) {
        self.eat_while(is_whitespace);
    }

    fn string_literal(&mut self) -> String {
        let mut lit = String::new();

        loop {
            let c = self.first();

            if c == '"' {
                // The last double quote is also consumed
                self.bump().unwrap();
                return lit;
            }

            lit.push(c);
            self.bump().unwrap();
        }
    }

    fn number(&mut self, first_digit: char) -> String {
        assert!(first_digit.is_ascii_digit());

        let mut result = first_digit.to_string();

        loop {
            let c = self.first();

            if c.is_ascii_digit() {
                result.push(c);
                self.bump().unwrap();
            } else {
                return result;
            }
        }
    }

    fn ident(&mut self, first: char) -> String {
        let mut ident = String::from(first);

        loop {
            let c = self.first();

            if is_id_continue(c) {
                ident.push(c);
            } else {
                return ident;
            }

            self.bump().unwrap();
        }
    }
}
