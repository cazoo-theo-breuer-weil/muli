use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{Cursor, Read, Seek, SeekFrom};
use super::{FilePosition, Span, Token, TokenType};

pub fn get_state_map() -> HashMap<LexerState, TokenType> {
    let mut state_token_map: HashMap<LexerState, TokenType> = HashMap::new();
    state_token_map.insert(LexerState::Integer, TokenType::Integer);
    state_token_map.insert(LexerState::Add, TokenType::Add);
    state_token_map.insert(LexerState::Whitespace, TokenType::Whitespace);

    state_token_map
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LexerState {
    Add,
    Blank,
    Integer,
    Whitespace,
}

struct Lexer {
    buffer: String,
    current_state: LexerState,
    span: Span,
    state_map: HashMap<LexerState, TokenType>,
}

impl Lexer {
    fn new() -> Lexer {
        Lexer {
            buffer: String::new(),
            current_state: LexerState::Blank,
            state_map: get_state_map(),
            span: Span::start(),
        }
    }

    pub fn lex(&mut self, stream: &mut (impl Read + Seek)) -> Result<Vec<Token>, &'static str> {
        let mut char_buffer = [0; 1];
        let mut tokens: Vec<Token> = Vec::new();

        while stream.read(&mut char_buffer).unwrap() > 0 {
            if self.transition(char_buffer[0] as char).is_err() {
                tokens.push(self.get_token()?);
                stream.seek(SeekFrom::Current(-1)).unwrap();
                self.reset();
            }
        }

        tokens.push(self.get_token()?);

        Ok(tokens
            .into_iter()
            .filter(|token| token.token_type != TokenType::Whitespace)
            .collect())
    }

    pub fn transition(&mut self, next_char: char) -> Result<(), ()> {
        if let Some(state) = self.get_next_state(next_char) {
            Ok(self.accept(state, next_char))
        } else {
            Err(())
        }
    }

    pub fn get_token(&mut self) -> Result<Token, &'static str> {
        match self.state_map.get(&self.current_state) {
            Some(token_type) => Ok(Token::new(self.span, *token_type, self.buffer.clone())),
            None => Err("invalid production"),
        }
    }

    pub fn reset(&mut self) {
        self.buffer.clear();
        self.current_state = LexerState::Blank;
        self.span.from = self.span.to;
    }

    fn accept(&mut self, state: LexerState, next_char: char) {
        self.buffer.push(next_char);
        self.current_state = state;
        self.span.to = match next_char {
            '\n' => self.span.to.next_line(),
            _ => self.span.to.next_char(),
        }
    }

    fn get_next_state(&self, c: char) -> Option<LexerState> {
        match self.current_state {
            LexerState::Add => None,
            LexerState::Blank => match c {
                '+' => Some(LexerState::Add),
                c if c.is_digit(10) => Some(LexerState::Integer),
                c if c.is_whitespace() => Some(LexerState::Whitespace),
                _ => None,
            },
            LexerState::Integer => match c {
                c if c.is_digit(10) => Some(LexerState::Integer),
                _ => None,
            },
            LexerState::Whitespace => match c {
                c if c.is_whitespace() => Some(LexerState::Whitespace),
                _ => None,
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer_add_expression_1() {
        let input = "3 + 5";
        let mut mock_file = Cursor::new(input);

        let expected = vec![
            Token::new(
                Span::new(FilePosition::new(0, 0), FilePosition::new(0, 1)),
                TokenType::Integer,
                "3".to_string(),
            ),
            Token::new(
                Span::new(FilePosition::new(0, 2), FilePosition::new(0, 3)),
                TokenType::Add,
                "+".to_string(),
            ),
            Token::new(
                Span::new(FilePosition::new(0, 4), FilePosition::new(0, 5)),
                TokenType::Integer,
                "5".to_string(),
            ),
        ];

        let mut lexer = Lexer::new();
        let actual = lexer.lex(&mut mock_file).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_lexer_add_expression_2() {
        let input = "4 + 11";
        let mut mock_file = Cursor::new(input);

        let expected = vec![
            Token::new(
                Span::new(FilePosition::new(0, 0), FilePosition::new(0, 1)),
                TokenType::Integer,
                "4".to_string(),
            ),
            Token::new(
                Span::new(FilePosition::new(0, 2), FilePosition::new(0, 3)),
                TokenType::Add,
                "+".to_string(),
            ),
            Token::new(
                Span::new(FilePosition::new(0, 4), FilePosition::new(0, 6)),
                TokenType::Integer,
                "11".to_string(),
            ),
        ];

        let mut lexer = Lexer::new();
        let actual = lexer.lex(&mut mock_file).unwrap();

        assert_eq!(actual, expected);
    }
}
