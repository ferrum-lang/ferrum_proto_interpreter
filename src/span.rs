#[derive(Clone, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Clone, Debug)]
pub struct Position {
    pub index: usize,

    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new() -> Self {
        return Self::from_position(Position::new());
    }

    pub fn from_position(position: Position) -> Self {
        return Self {
            start: position.clone(),
            end: position,
        };
    }
}

impl Position {
    pub fn new() -> Self {
        return Self {
            index: 0,
            line: 1,
            column: 0,
        };
    }

    pub fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}
