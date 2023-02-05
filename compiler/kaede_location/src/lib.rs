pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone)]
pub struct SpanBuilder {
    current: Location,

    start: Option<Location>,
}

impl SpanBuilder {
    pub fn new() -> Self {
        Self {
            current: Location::new(),
            start: None,
        }
    }

    pub fn increme_line(&mut self) {
        self.current.increme_line();
    }

    pub fn increme_column(&mut self) {
        self.current.increme_column();
    }

    pub fn start(&mut self) {
        self.start = Some(self.current.clone());
    }

    pub fn build(&self) -> Span {
        assert!(self.start.is_some());

        Span::new(self.start.clone().unwrap(), self.current.clone())
    }
}

impl Default for SpanBuilder {
    fn default() -> Self {
        Self::new()
    }
}

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
