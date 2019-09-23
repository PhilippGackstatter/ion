use crate::types::Token;
use crate::types::TokenKind::*;

#[derive(Default)]
pub struct Lexer {
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            tokens: vec![
                Token::new_debug(Num(0)),
                Token::new_debug(Plus),
                Token::new_debug(Num(4)),
                Token::new_debug(Star),
                Token::new_debug(Num(8)),
                Token::new_debug(EndOfFile),
            ],
        }
    }

    pub fn new_from_tokens(tokens: Vec<Token>) -> Self {
        Lexer { tokens }
    }
}
