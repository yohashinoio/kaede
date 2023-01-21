use super::{cursor::Cursor, token::TokenKind};

pub fn lex(input: &str) -> Vec<TokenKind> {
    let mut cursor = Cursor::new(input);

    let mut result = Vec::new();

    loop {
        match cursor.advance_token() {
            TokenKind::Eof => {
                result.push(TokenKind::Eof);
                break;
            }
            tok => result.push(tok),
        }
    }

    result
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

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> TokenKind {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return TokenKind::Eof,
        };

        let token = match first_char {
            c if is_whitespace(c) => {
                self.eat_whitespace();
                self.advance_token()
            }

            c @ '0'..='9' => TokenKind::Integer(self.number(c)),

            _ => unreachable!(),
        };

        token
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
}
