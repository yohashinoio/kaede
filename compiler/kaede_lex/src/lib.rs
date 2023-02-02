use cursor::Cursor;
use kaede_location::{Location, Span};
use token::{Token, TokenKind};

mod cursor;
mod location;
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
    fn advance_token(&mut self) -> Token {
        let start = self.get_loc().clone();

        let with_span = |t: TokenKind, finish: &Location| Token {
            kind: t,
            span: Span::new(start, finish.clone()),
        };

        let first_char = match self.bump() {
            Some(c) => c,
            None => return with_span(TokenKind::Eof, self.get_loc()),
        };

        match first_char {
            // Skipper
            c if is_whitespace(c) => {
                self.eat_whitespace();
                self.advance_token()
            }

            // Number
            c @ '0'..='9' => with_span(TokenKind::Integer(self.number(c)), self.get_loc()),

            // Identifier or reserved words
            c if is_id_start(c) => {
                let ident = self.ident(c);

                match ident.as_str() {
                    "fn" => with_span(TokenKind::Function, self.get_loc()),
                    _ => with_span(TokenKind::Ident(ident), self.get_loc()),
                }
            }

            // Punctuators
            '(' => with_span(TokenKind::OpenParen, self.get_loc()),
            ')' => with_span(TokenKind::CloseParen, self.get_loc()),
            '{' => with_span(TokenKind::OpenBrace, self.get_loc()),
            '}' => with_span(TokenKind::CloseBrace, self.get_loc()),
            ',' => with_span(TokenKind::Comma, self.get_loc()),

            // Operators
            '+' => with_span(TokenKind::Add, self.get_loc()),
            '-' => with_span(TokenKind::Sub, self.get_loc()),
            '*' => with_span(TokenKind::Mul, self.get_loc()),
            '/' => with_span(TokenKind::Div, self.get_loc()),

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
