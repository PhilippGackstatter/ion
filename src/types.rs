use std::fmt;

pub type Value = i32;

#[derive(Debug)]
#[repr(u8)]
pub enum Bytecode {
    OpMul,
    OpAdd,
    OpDiv,
    OpSub,
    OpNot,
    OpEqual,
    OpNegate,
    OpConstant,
    OpReturn,
}

#[derive(Default, Debug)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            constants: vec![],
            code: vec![],
        }
    }
}

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
    #[allow(dead_code)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Semicolon,
    LeftParen,
    RightParen,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Bang,
    Negate,
    Plus,
    Minus,
    Star,
    Slash,
    Num(i32),
    String_(String),
    True_,
    False_,
    EndOfFile,
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Star => "*",
            Slash => "/",
            Plus => "+",
            Minus => "-",
            _ => "",
        };
        write!(f, "{}", symbol)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binary(rexpr, token, lexpr) => write!(f, "{} {} {}", *rexpr, token.kind, *lexpr),
            Unary(token, lexpr) => write!(f, "{} {}", token.kind, *lexpr),
            Number(int) => write!(f, "{}", int),
            Str(str_) => write!(f, "{}", str_),
            False => write!(f, "false"),
            True => write!(f, "true"),
        }
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\nConstants")?;
        for (i, con) in self.constants.iter().enumerate() {
            writeln!(f, "{} -> {}", i, con)?;
        }

        writeln!(f, "\nBytecode")?;
        let mut iter = self.code.iter();
        while let Some(str_) = byte_to_opcode(&mut iter, &self.constants) {
            writeln!(f, "{}", str_)?;
        }
        write!(f, "")
    }
}

fn byte_to_opcode(bytes: &mut std::slice::Iter<u8>, constants: &[Value]) -> Option<String> {
    if let Some(byte) = bytes.next() {
        let res = match byte {
            0 => format!("{:?}", Bytecode::OpMul),
            1 => format!("{:?}", Bytecode::OpAdd),
            2 => format!("{:?}", Bytecode::OpDiv),
            3 => format!("{:?}", Bytecode::OpSub),
            4 => format!("{:?}", Bytecode::OpNot),
            5 => format!("{:?}", Bytecode::OpEqual),
            6 => format!("{:?}", Bytecode::OpNegate),
            7 => {
                let b1 = bytes.next().unwrap();
                let b2 = bytes.next().unwrap();
                let index = u16::from(*b1) << 8 | u16::from(*b2);
                format!("{:?} {}", Bytecode::OpConstant, constants[index as usize])
            }
            8 => format!("{:?}", Bytecode::OpReturn),
            _ => unreachable!(),
        };
        Some(res)
    } else {
        None
    }
}
