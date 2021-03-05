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

fn get_next_lexer_state(state: LexerState, character: char) -> Option<LexerState> {
    match state {
        LexerState::Add => None,
        LexerState::Blank => match character {
            '+' => Some(LexerState::Add),
            character if character.is_digit(10) => Some(LexerState::Integer),
            character if character.is_whitespace() => Some(LexerState::Whitespace),
            _ => None,
        },
        LexerState::Integer => match character {
            character if character.is_digit(10) => Some(LexerState::Integer),
            _ => None,
        },
        LexerState::Whitespace => match character {
            character if character.is_whitespace() => Some(LexerState::Whitespace),
            _ => None,
        },
    }
}

pub fn stream_to_tokens(stream: &mut (impl Seek + Read)) -> Vec<Token> {
    let mut state_token_map: HashMap<LexerState, TokenType> = HashMap::new();
    state_token_map.insert(LexerState::Integer, TokenType::Integer);
    state_token_map.insert(LexerState::Add, TokenType::Add);
    state_token_map.insert(LexerState::Whitespace, TokenType::Whitespace);

    let mut buffer = [0; 1];
    let mut lexer_state = LexerState::Blank;
    let mut tokens = Vec::new();
    let mut token_buffer = String::new();

    loop {
        if stream.read(&mut buffer).unwrap() == 0 {
            match state_token_map.get(&lexer_state) {
                Some(token_type) => {
                    tokens.push(Token::new(*token_type, token_buffer.clone()));
                    token_buffer.clear();
                    lexer_state = LexerState::Blank;
                }
                None => {
                    println!("invalid token");
                }
            }

            break
        }

        let character = buffer[0] as char;

        match get_next_lexer_state(lexer_state, character) {
            Some(next_state) => {
                lexer_state = next_state;
                token_buffer.push(character);
            },
            None => {
                match state_token_map.get(&lexer_state) {
                    Some(token_type) => {
                        tokens.push(Token::new(*token_type, token_buffer.clone()));
                        token_buffer.clear();
                        lexer_state = LexerState::Blank;
                        stream.seek(SeekFrom::Current(-1)).unwrap();
                    }
                    None => {
                        println!("invalid token");
                    }
                }

            },
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
