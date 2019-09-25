use crate::types::Token;
use crate::types::TokenKind::{self, *};

#[derive(Default)]
pub struct Lexer {
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new1() -> Self {
        // print 8 + 3 * 4 - 10 / 2;
        let tokens = vec![
            PrintToken,
            Num(8),
            Plus,
            Num(3),
            Star,
            Num(4),
            Minus,
            Num(10),
            Slash,
            Num(2),
            Semicolon,
            PrintToken,
            Num(55),
            Semicolon,
            EndOfFile,
        ];
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new2() -> Self {
        // if (6 > 2) print 5; else print 9;
        let tokens = vec![
            IfToken,
            LeftParen,
            Num(1),
            Greater,
            Num(2),
            RightParen,
            PrintToken,
            Num(5),
            Semicolon,
            ElseToken,
            PrintToken,
            Num(9),
            Semicolon,
            EndOfFile,
        ];
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new3() -> Self {
        // var x = 5;
        // print x;
        let tokens = vec![
            VarToken,
            IdToken("x".to_owned()),
            Equal,
            Num(5),
            Semicolon,
            PrintToken,
            IdToken("x".to_owned()),
            Semicolon,
            EndOfFile,
        ];
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new4() -> Self {
        // var i = 0;
        // while (i < 5) x = x + 1;
        // print i;
        let tokens = vec![
            VarToken,
            IdToken("i".to_owned()),
            Equal,
            Num(0),
            Semicolon,
            WhileToken,
            LeftParen,
            IdToken("i".to_owned()),
            Less,
            Num(5),
            RightParen,
            IdToken("i".to_owned()),
            Equal,
            IdToken("i".to_owned()),
            Plus,
            Num(1),
            Semicolon,
            PrintToken,
            IdToken("i".to_owned()),
            Semicolon,
            EndOfFile,
        ];
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new_from_tokens(tokens: Vec<Token>) -> Self {
        Lexer { tokens }
    }

    pub fn new_from_tokenkind(tokens: Vec<TokenKind>) -> Self {
        Lexer {
            tokens: tokens
                .iter()
                .map(|tk| Token::new_debug(tk.clone()))
                .collect(),
        }
    }
}
