/// Record the start location with the `start` method,
/// then increase the location with `increase*` methods
///
/// The `build` method creates a span from start to the current position
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

    pub fn increase_line(&mut self) {
        self.current.increase_line();
    }

    pub fn increase_column(&mut self) {
        self.current.increase_column();
    }

    /// Record a start position
    pub fn start(&mut self) {
        self.start = Some(self.current);
    }

    pub fn build(&self) -> Span {
        assert!(self.start.is_some());

        Span::new(self.start.unwrap(), self.current)
    }
}

impl Default for SpanBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start: Location,
    pub finish: Location,
}

impl Span {
    pub fn new(start: Location, finish: Location) -> Self {
        Self { start, finish }
    }

    pub fn dummy() -> Self {
        Self {
            start: Location::dummy(),
            finish: Location::dummy(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Location {
    pub line: i32,
    pub column: i32,
}

impl Default for Location {
    fn default() -> Self {
        Self::new()
    }
}

impl Location {
    pub fn dummy() -> Self {
        Self {
            line: -1,
            column: -1,
        }
    }

    pub fn new() -> Self {
        Self { line: 1, column: 1 }
    }

    pub fn increase_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn increase_column(&mut self) {
        self.column += 1;
    }
}
