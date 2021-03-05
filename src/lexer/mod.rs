use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{Cursor, Read, Seek, SeekFrom};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    Integer,
    Add,
    Whitespace,
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    value: String,
}

impl Token {
    fn new(token_type: TokenType, value: String) -> Token {
        Token { token_type, value }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        other.token_type == self.token_type && other.value == self.value
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LexerState {
    Add,
    Blank,
    Integer,
    Whitespace,
}

struct StateMachine {
    buffer: String,
    current_state: LexerState,
    state_map: HashMap<LexerState, TokenType>,
}

impl StateMachine {
    fn new() -> StateMachine {
        StateMachine {
            buffer: String::new(),
            current_state: LexerState::Blank,
            state_map: get_state_map(),
        }
    }

    pub fn accept(&mut self, next_char: char) -> bool {
        match self.get_next_state(next_char) {
            Some(next_state) => {
                self.current_state = next_state;
                self.buffer.push(next_char);
                true
            }
            None => false,
        }
    }

    pub fn produce(&mut self) -> Result<Token, &'static str> {
        match self.state_map.get(&self.current_state) {
            Some(token_type) => {
                let token = Token::new(*token_type, self.buffer.clone());
                self.reset();
                Ok(token)
            }
            None => Err("invalid production"),
        }
    }

    fn reset(&mut self) {
        self.current_state = LexerState::Blank;
        self.buffer.clear();
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

pub fn get_state_map() -> HashMap<LexerState, TokenType> {
    let mut state_token_map: HashMap<LexerState, TokenType> = HashMap::new();
    state_token_map.insert(LexerState::Integer, TokenType::Integer);
    state_token_map.insert(LexerState::Add, TokenType::Add);
    state_token_map.insert(LexerState::Whitespace, TokenType::Whitespace);

    state_token_map
}

pub fn stream_to_tokens(stream: &mut (impl Seek + Read)) -> Vec<Token> {
    let mut state_machine = StateMachine::new();
    let mut buffer = [0; 1];
    let mut tokens: Vec<Token> = Vec::new();

    loop {
        if stream.read(&mut buffer).unwrap() == 0 {
            match state_machine.produce() {
                Ok(token) => {
                    tokens.push(token);
                }
                Err(error) => {
                    println!("{}", error);
                }
            }

            break;
        }

        if !state_machine.accept(buffer[0] as char) {
            match state_machine.produce() {
                Ok(token) => {
                    tokens.push(token);
                    stream.seek(SeekFrom::Current(-1)).unwrap();
                }
                Err(error) => {
                    panic!("{}", error);
                }
            }
        }
    }

    tokens
        .into_iter()
        .filter(|token| token.token_type != TokenType::Whitespace)
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer_add_expression_1() {
        let input = "3 + 5";
        let mut mock_file = Cursor::new(input);

        let expected = vec![
            Token::new(TokenType::Integer, "3".to_string()),
            Token::new(TokenType::Add, "+".to_string()),
            Token::new(TokenType::Integer, "5".to_string()),
        ];

        let actual = stream_to_tokens(&mut mock_file);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_lexer_add_expression_2() {
        let input = "4 + 11";
        let mut mock_file = Cursor::new(input);

        let expected = vec![
            Token::new(TokenType::Integer, "4".to_string()),
            Token::new(TokenType::Add, "+".to_string()),
            Token::new(TokenType::Integer, "11".to_string()),
        ];

        let actual = stream_to_tokens(&mut mock_file);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_tokens_are_equal_when_equal() {
        let a = Token::new(TokenType::Integer, "3".to_string());
        let b = Token::new(TokenType::Integer, "3".to_string());
        assert_eq!(a, b);
    }

    #[test]
    fn test_tokens_are_not_equal_when_value_not_equal() {
        let a = Token::new(TokenType::Integer, "3".to_string());
        let b = Token::new(TokenType::Integer, "4".to_string());
        assert_ne!(a, b);
    }

    #[test]
    fn test_tokens_are_not_equal_when_type_not_equal() {
        let a = Token::new(TokenType::Integer, "3".to_string());
        let b = Token::new(TokenType::Add, "3".to_string());
        assert_ne!(a, b);
    }
}
