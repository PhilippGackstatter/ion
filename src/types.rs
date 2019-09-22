use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    /// The offset in bytes in the source file where the lexeme begins
    pub offset: u32,
    /// The length of the lexeme
    pub length: u8,
    /// The kind of the token
    pub kind: TokenKind,
}

impl Token {
    pub fn new(offset: u32, length: u8, kind: TokenKind) -> Self {
        Token {
            offset,
            length,
            kind,
        }
    }

    pub fn new_debug(kind: TokenKind) -> Self {
        Token {
            offset: 0,
            length: 0,
            kind,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    LEFT_PAREN,
    RIGHT_PAREN,
    BANG_EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    BANG,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    NUMBER(i32),
    STRING(String),
    TRUE,
    FALSE,
    EOF
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(Box<Expression>, Token, Box<Expression>),
    // Assign(Token, Box<Expression>),
    Unary(Token, Box<Expression>),
    Number(i32),
    Str(String),
    False,
    True,
}

use Expression::*;

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binary(rexpr, token, lexpr) => {
                write!(f, "{} {:?} ({})", *rexpr, token.kind, *lexpr)
            },
            Unary(token, lexpr) => {
                write!(f, "{:?} ({})", token.kind, *lexpr)
            },
            Number(int) => write!(f, "{}", int),
            Str(str_) => write!(f, "{}", str_),
            False => write!(f, "false"),
            True => write!(f, "true"),
        }
    }   
}