use std::fmt;
use std::ops::Range;

pub type Program = Vec<Declaration>;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    Int(i32),
    Double(f32),
    Obj(Object),
}

impl Value {
    pub fn unwrap_bool(&self) -> bool {
        if let Value::Bool(b) = self {
            *b
        } else {
            panic!("Expected bool.");
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    StringObj(String),
    FnObj(String, Chunk, u8),
}

#[derive(Debug)]
#[repr(u8)]
pub enum Bytecode {
    OpPop,
    OpMul,
    OpAdd,
    OpDiv,
    OpSub,
    OpNot,
    OpEqual,
    OpNegate,
    OpGreater,
    OpLess,
    OpGreaterEqual,
    OpLessEqual,
    OpConstant,
    OpDefineGlobal,
    OpSetGlobal,
    OpGetGlobal,
    OpSetLocal,
    OpGetLocal,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
    OpCall,
    OpPrint,
    OpReturn,
}

#[derive(Default, Debug, PartialEq, Clone)]
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

    pub fn is_id_token(&self) -> bool {
        if let TokenKind::IdToken(_) = self.kind {
            true
        } else {
            false
        }
    }

    pub fn get_id(&self) -> String {
        if let TokenKind::IdToken(id) = &self.kind {
            id.clone()
        } else {
            panic!("Expected IdToken");
        }
    }
}

impl Into<std::ops::Range<usize>> for Token {
    fn into(self) -> std::ops::Range<usize> {
        let off = self.offset as usize;
        off..(off + (self.length as usize))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Semicolon,
    Colon,
    Arrow,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Num(i32),
    FloatNum(f32),
    String_(String),
    TrueToken,
    FalseToken,
    For,
    VarToken,
    WhileToken,
    StructToken,
    IdToken(String),
    PrintToken,
    IfToken,
    ElseToken,
    And,
    Or,
    Class,
    Fun,
    Super,
    This,
    Return,
    EndOfFile,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    StatementDecl(Statement),
    VarDecl(String, Expression),
    StructDecl(Token, Vec<(Token, Token)>),
    FnDecl(String, Vec<(Token, Token)>, Option<Token>, Statement),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Print(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Ret(Option<Expression>),
    Block(Vec<Declaration>),
    ExpressionStmt(Expression),
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Binary(Box<Expression>, Token, Box<Expression>),
    Assign(String, Box<Expression>),
    Unary(Token, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Integer {
        int: i32,
    },
    Double { float: f32 },
    Str {
        string: String,
    },
    Identifier(String),
    False,
    True,
}

#[derive(Debug)]
pub struct Expression {
    pub tokens: Range<usize>,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn new(tokens: Range<usize>, kind: ExpressionKind) -> Self {
        Expression {
            tokens, kind
        }
    }

    pub fn new_debug(kind: ExpressionKind) -> Self {
        Expression {
            tokens: 0..1, kind
        }
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(int) => write!(f, "{}", int),
            Value::Double(float) => write!(f, "{}", float),
            Value::Obj(obj) => write!(f, "{}", obj),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::StringObj(str_) => write!(f, "{}", str_),
            Object::FnObj(name, _chunk, arity) => write!(f, "{} (args: {})", name, arity),
        }
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\nChunk")?;
        writeln!(f, "=====")?;
        writeln!(f, "\nConstants")?;
        for (i, con) in self.constants.iter().enumerate() {
            writeln!(f, "{} -> {}", i, con)?;
        }

        writeln!(f, "\nBytecode")?;
        let mut iter = self.code.iter().enumerate();
        while let Some((i, str_)) = byte_to_opcode(&mut iter, &self.constants) {
            writeln!(f, "{} {}", i, str_)?;
        }
        Ok(())
    }
}

fn byte_to_opcode(
    bytes: &mut std::iter::Enumerate<std::slice::Iter<u8>>,
    constants: &[Value],
) -> Option<(usize, String)> {
    if let Some((index, byte_)) = bytes.next() {
        let byte = unsafe { std::mem::transmute::<u8, Bytecode>(*byte_) };
        let res = match byte {
            Bytecode::OpPop => format!("{:?}", byte),
            Bytecode::OpMul => format!("{:?}", byte),
            Bytecode::OpAdd => format!("{:?}", byte),
            Bytecode::OpDiv => format!("{:?}", byte),
            Bytecode::OpSub => format!("{:?}", byte),
            Bytecode::OpNot => format!("{:?}", byte),
            Bytecode::OpEqual => format!("{:?}", byte),
            Bytecode::OpNegate => format!("{:?}", byte),
            Bytecode::OpGreater => format!("{:?}", byte),
            Bytecode::OpLess => format!("{:?}", byte),
            Bytecode::OpGreaterEqual => format!("{:?}", byte),
            Bytecode::OpLessEqual => format!("{:?}", byte),
            Bytecode::OpPrint => format!("{:?}", byte),
            Bytecode::OpConstant => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            Bytecode::OpDefineGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            Bytecode::OpSetGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            Bytecode::OpGetGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            Bytecode::OpSetLocal => {
                let index = read_u8(bytes);
                format!("{:?} {}", byte, index)
            }
            Bytecode::OpGetLocal => {
                let index = read_u8(bytes);
                format!("{:?} {}", byte, index)
            }
            Bytecode::OpJumpIfFalse => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            Bytecode::OpJump => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            Bytecode::OpLoop => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            Bytecode::OpReturn => {
                let retvals = read_u8(bytes);
                let pop = read_u8(bytes);
                format!("{:?} {} vals, pop {}", byte, retvals, pop)
            }
            Bytecode::OpCall => format!("{:?}", byte),
        };
        Some((index, res))
    } else {
        None
    }
}

fn read_u8(bytes: &mut std::iter::Enumerate<std::slice::Iter<u8>>) -> u8 {
    let (_, b1) = bytes.next().unwrap();
    *b1
}

fn read_u16(bytes: &mut std::iter::Enumerate<std::slice::Iter<u8>>) -> u16 {
    let (_, b1) = bytes.next().unwrap();
    let (_, b2) = bytes.next().unwrap();
    u16::from(*b1) << 8 | u16::from(*b2)
}
