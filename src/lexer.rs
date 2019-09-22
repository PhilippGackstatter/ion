use crate::types::Token;
use crate::types::TokenKind::*;

pub struct Lexer {
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            tokens: vec![
                Token::new_debug(NUMBER(0)),
                Token::new_debug(PLUS),
                Token::new_debug(NUMBER(4)),
                Token::new_debug(STAR),
                Token::new_debug(NUMBER(8)),
                Token::new_debug(EOF),
            ]
        }
    }

    pub fn new_from_tokens(tokens: Vec<Token>) -> Self {
        Lexer {
            tokens
        }
    }
}