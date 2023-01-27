use super::{cursor::Cursor, token::TokenKind};

pub fn lex(input: &str) -> impl Iterator<Item = TokenKind> + '_ {
    let mut cursor = Cursor::new(input);

    std::iter::from_fn(move || {
        let token = cursor.advance_token();

        match token {
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
    fn advance_token(&mut self) -> TokenKind {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return TokenKind::Eof,
        };

        match first_char {
            // Skipper
            c if is_whitespace(c) => {
                self.eat_whitespace();
                self.advance_token()
            }

            // Number
            c @ '0'..='9' => TokenKind::Integer(self.number(c)),

            // Identifier or reserved words
            c if is_id_start(c) => {
                let ident = self.ident(c);

                match ident.as_str() {
                    "fn" => TokenKind::Function,
                    _ => TokenKind::Ident(ident),
                }
            }

            // Punctuators
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            ',' => TokenKind::Comma,

            // Operators
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,

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
