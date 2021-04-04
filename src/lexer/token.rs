use std::cmp::PartialEq;
use std::fmt::Debug;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FilePosition {
    pub column: u8,
    pub line: u8,
}

impl FilePosition {
    pub fn new(line: u8, column: u8) -> FilePosition {
        FilePosition { column, line }
    }

    pub fn start() -> FilePosition {
        FilePosition::new(0, 0)
    }

    pub fn next_char(&self) -> FilePosition {
        FilePosition {
            column: self.column + 1,
            line: self.line,
        }
    }

    pub fn next_line(&self) -> FilePosition {
        FilePosition {
            column: 0,
            line: self.line + 1,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub from: FilePosition,
    pub to: FilePosition,
}

impl Span {
    pub fn new(from: FilePosition, to: FilePosition) -> Span {
        Span { from, to }
    }

    pub fn start() -> Span {
        Span::new(FilePosition::start(), FilePosition::start())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Assignment,
    Comma,
    FunctionBody,
    FunctionParameters,
    Identifier(String),
    Integer(String),
    LeftBracket,
    PropertyAccess,
    RightBracket,
    Semicolon,
    TypeDeclaration,
    VariableDeclaration,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub span: Span,
    pub token_type: TokenType,
}

impl Token {
    pub fn new(span: Span, of_type: TokenType) -> Token {
        Token { span, token_type: of_type }
    }
}
