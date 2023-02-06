use cursor::Cursor;
use token::{Token, TokenKind};

mod cursor;
pub mod token;

#[cfg(test)]
mod tests;

pub fn lex(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);

    std::iter::from_fn(move || {
        let token = cursor.advance_token();

        match token.kind {
            TokenKind::Eof => None,
            _ => Some(token),
        }
    })
}

fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
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

/// True if `c` is valid as a first character of an identifier.
fn is_id_start(c: char) -> bool {
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier.
fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

impl Cursor<'_> {
    /// Add span
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
            None => return self.create_token(TokenKind::Eof),
        };

        match first_char {
            // Skipper
            c if is_whitespace(c) => {
                self.eat_whitespace();
                self.advance_token()
            }

            // Number
            c @ '0'..='9' => {
                let n = self.number(c);
                self.create_token(TokenKind::Integer(n))
            }

            // Identifier or reserved words
            c if is_id_start(c) => {
                let ident = self.ident(c);

                match ident.as_str() {
                    "fn" => self.create_token(TokenKind::Function),
                    "return" => self.create_token(TokenKind::Return),
                    _ => self.create_token(TokenKind::Ident(ident)),
                }
            }

            // Punctuators
            '(' => self.create_token(TokenKind::OpenParen),
            ')' => self.create_token(TokenKind::CloseParen),
            '{' => self.create_token(TokenKind::OpenBrace),
            '}' => self.create_token(TokenKind::CloseBrace),
            ',' => self.create_token(TokenKind::Comma),
            ';' => self.create_token(TokenKind::Semi),

            // Operators
            '+' => self.create_token(TokenKind::Add),
            '-' => self.create_token(TokenKind::Sub),
            '*' => self.create_token(TokenKind::Mul),
            '/' => self.create_token(TokenKind::Div),

            _ => unreachable!(),
        }
    }

    fn eat_whitespace(&mut self) {
        self.eat_while(is_whitespace);
    }

    fn number(&mut self, first_digit: char) -> u64 {
        let mut result = first_digit
            .to_digit(10)
            .expect("The caller should have passed a digit.") as u64;

        loop {
            let c = self.first();

            match c.to_digit(10) {
                Some(digit) => result = result * 10 + (digit as u64),
                None => break,
            }

            self.bump();
        }

        result
    }

    fn ident(&mut self, first: char) -> String {
        let mut ident = String::from(first);

        loop {
            let c = self.first();

            if is_id_continue(c) {
                ident.push(c);
            } else {
                break;
            }

            self.bump();
        }

        ident
    }
}
