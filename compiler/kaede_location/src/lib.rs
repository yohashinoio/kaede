pub type Spanned<T> = (T, Span);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub start: Location,
    pub finish: Location,
}

impl Span {
    pub fn new(start: Location, finish: Location) -> Self {
        Self { start, finish }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl Default for Location {
    fn default() -> Self {
        Self::new()
    }
}

impl Location {
    pub fn new() -> Self {
        Self { line: 1, column: 1 }
    }

    pub fn increme_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn increme_column(&mut self) {
        self.column += 1;
    }
}
