use std::cmp::PartialEq;
use std::fmt::Debug;
use std::io::{Cursor, Read, Seek, SeekFrom};

use super::{Span, Token, TokenType};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LexerState {
    Assignment,
    Blank,
    Comma,
    LeftBracket,
    Lexeme,
    Integer,
    PropertyAccess,
    RightBracket,
    Semicolon,
    StartFunctionBody,
    FunctionBody,
    Whitespace,
}

struct Lexer {
    buffer: String,
    current_state: LexerState,
    span: Span,
}

impl Lexer {
    fn new() -> Lexer {
        Lexer {
            buffer: String::new(),
            current_state: LexerState::Blank,
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

    fn reset(&mut self) {
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
            LexerState::Assignment => None,
            LexerState::Comma => None,
            LexerState::FunctionBody => None,
            LexerState::Blank => match c {
                '=' => Some(LexerState::Assignment),
                '.' => Some(LexerState::PropertyAccess),
                ';' => Some(LexerState::Semicolon),
                '_' => Some(LexerState::Lexeme),
                '-' => Some(LexerState::StartFunctionBody),
                '(' => Some(LexerState::LeftBracket),
                ')' => Some(LexerState::RightBracket),
                ',' => Some(LexerState::Comma),
                c if c.is_alphabetic() => Some(LexerState::Lexeme),
                c if c.is_digit(10) => Some(LexerState::Integer),
                c if c.is_whitespace() => Some(LexerState::Whitespace),
                _ => None,
            },
            LexerState::Integer => match c {
                c if c.is_digit(10) => Some(LexerState::Integer),
                _ => None,
            },
            LexerState::LeftBracket => None,
            LexerState::Lexeme => match c {
                c if c.is_alphanumeric() => Some(LexerState::Lexeme),
                '_' => Some(LexerState::Lexeme),
                _ => None,
            },
            LexerState::PropertyAccess => None,
            LexerState::RightBracket => None,
            LexerState::StartFunctionBody => match c {
                '>' => Some(LexerState::FunctionBody),
                _ => None,
            },
            LexerState::Semicolon => None,
            LexerState::Whitespace => match c {
                c if c.is_whitespace() => Some(LexerState::Whitespace),
                _ => None,
            },
        }
    }

    fn get_token(&mut self) -> Result<Token, &'static str> {
        let token_type = match self.current_state {
            LexerState::Assignment => Some(TokenType::Assignment),
            LexerState::Blank => None,
            LexerState::Comma => Some(TokenType::Comma),
            LexerState::FunctionBody => Some(TokenType::FunctionBody),
            LexerState::Integer => Some(TokenType::Integer(self.buffer.clone())),
            LexerState::LeftBracket => Some(TokenType::LeftBracket),
            LexerState::Lexeme => match self.buffer.as_str() {
                "fn" => Some(TokenType::FunctionParameters),
                "type" => Some(TokenType::TypeDeclaration),
                "var" => Some(TokenType::VariableDeclaration),
                _ => Some(TokenType::Identifier(self.buffer.clone())),
            },
            LexerState::PropertyAccess => Some(TokenType::PropertyAccess),
            LexerState::RightBracket => Some(TokenType::RightBracket),
            LexerState::Semicolon => Some(TokenType::Semicolon),
            LexerState::StartFunctionBody => None,
            LexerState::Whitespace => Some(TokenType::Whitespace),
        };

        match token_type {
            Some(token_type) => Ok(Token::new(self.span, token_type)), 
            None => Err("invalid production"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    
    fn map_result(result: Result<Vec<Token>, &'static str>) -> Vec<TokenType> {
        result.unwrap().into_iter().map(|token| token.token_type).collect()
    }

    #[test]
    fn simple_assignment() {
        let program = &mut Cursor::new("var variable = 3;");

        let expected = vec![
            TokenType::VariableDeclaration,
            TokenType::Identifier("variable".to_string()),
            TokenType::Assignment,
            TokenType::Integer("3".to_string()),
            TokenType::Semicolon,
        ];

        let mut lexer = Lexer::new();
        let actual = map_result(lexer.lex(program));
        assert_eq!(actual, expected);
    }

    #[test]
    fn leading_underscore() {
        let program = &mut Cursor::new("var _variable = 22;");

        let expected = vec![
            TokenType::VariableDeclaration,
            TokenType::Identifier("_variable".to_string()),
            TokenType::Assignment,
            TokenType::Integer("22".to_string()),
            TokenType::Semicolon,
        ];

        let mut lexer = Lexer::new();
        let actual = map_result(lexer.lex(program));
        assert_eq!(actual, expected);
    }

    #[test]
    fn interpolated_underscore() {
        let program = &mut Cursor::new("var _var_iable = 456;");

        let expected = vec![
            TokenType::VariableDeclaration,
            TokenType::Identifier("_var_iable".to_string()),
            TokenType::Assignment,
            TokenType::Integer("456".to_string()),
            TokenType::Semicolon,
        ];

        let mut lexer = Lexer::new();
        let actual = map_result(lexer.lex(program));
        assert_eq!(actual, expected);
    }

    #[test]
    fn property_access() {
        let program = &mut Cursor::new("var that = this.thing;");

        let expected = vec![
            TokenType::VariableDeclaration,
            TokenType::Identifier("that".to_string()),
            TokenType::Assignment,
            TokenType::Identifier("this".to_string()),
            TokenType::PropertyAccess,
            TokenType::Identifier("thing".to_string()),
            TokenType::Semicolon,
        ];

        let mut lexer = Lexer::new();
        let actual = map_result(lexer.lex(program));
        assert_eq!(actual, expected);
    }

    #[test]
    fn type_declaration() {
        let program = &mut Cursor::new("type newtype = int;");

        let expected = vec![
            TokenType::TypeDeclaration,
            TokenType::Identifier("newtype".to_string()),
            TokenType::Assignment,
            TokenType::Identifier("int".to_string()),
            TokenType::Semicolon,
        ];

        let mut lexer = Lexer::new();
        let actual = map_result(lexer.lex(program));
        assert_eq!(actual, expected);
    }

    #[test]
    fn function_declaration() {
        let program = &mut Cursor::new("fn (a, b) -> 4;");

        let expected = vec![
            TokenType::FunctionParameters,
            TokenType::LeftBracket,
            TokenType::Identifier("a".to_string()),
            TokenType::Comma,
            TokenType::Identifier("b".to_string()),
            TokenType::RightBracket,
            TokenType::FunctionBody,
            TokenType::Integer("4".to_string()),
            TokenType::Semicolon,
        ];

        let mut lexer = Lexer::new();
        let actual = map_result(lexer.lex(program));

        assert_eq!(actual, expected);
    }

    #[test]
    fn small_program() {
        let program = &mut Cursor::new("\
            var add = fn (a, b) -> a.add(b);\n\
            var result = add(4, 5)\n\
            ");

        let mut lexer = Lexer::new();
        let actual = lexer.lex(program);
        assert!(actual.is_ok());
    }
}
