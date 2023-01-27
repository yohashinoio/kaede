use std::str::Chars;

pub const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
        }
    }

    pub fn first(&self) -> char {
        self.chars.clone().nth(0).unwrap_or(EOF_CHAR)
    }

    // pub fn second(&self) -> char {
    //     self.chars.clone().nth(1).unwrap_or(EOF_CHAR)
    // }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty() || self.first() == EOF_CHAR
    }

    pub fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }
}
