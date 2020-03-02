use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;

pub type Program = Vec<Declaration>;

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Int(i32),
    Double(f32),
    Obj(Rc<RefCell<Object>>),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Value::Bool(boolean) => write!(f, "{:?}", boolean),
            Value::Int(int) => write!(f, "{:?}", int),
            Value::Double(double) => write!(f, "{:?}", double),
            Value::Obj(obj) => write!(f, "{:?}", obj.borrow().clone()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(lboolean), Value::Bool(rboolean)) => lboolean == rboolean,
            (Value::Int(lint), Value::Int(rint)) => lint == rint,
            (Value::Double(ldouble), Value::Double(rdouble)) => ldouble == rdouble,
            (Value::Obj(lobj), Value::Obj(robj)) => lobj.borrow().clone() == robj.borrow().clone(),
            (_, _) => false,
        }
    }
}

impl Value {
    pub fn unwrap_bool(self) -> bool {
        if let Value::Bool(b) = self {
            b
        } else {
            panic!("Expected bool.");
        }
    }

    pub fn unwrap_obj(self) -> Rc<RefCell<Object>> {
        if let Value::Obj(obj) = self {
            obj
        } else {
            panic!("Expected Object.");
        }
    }

    pub fn unwrap_int(self) -> i32 {
        if let Value::Int(int) = self {
            int
        } else {
            panic!("Expected i32.");
        }
    }

    pub fn unwrap_double(self) -> f32 {
        if let Value::Double(double) = self {
            double
        } else {
            panic!("Expected f32.");
        }
    }

    pub fn unwrap_string(self) -> String {
        if let Object::StringObj(str_) = self.unwrap_obj().borrow().clone() {
            str_
        } else {
            panic!("Expected string");
        }
    }

    pub fn unwrap_struct(self) -> HashMap<String, Value> {
        let obj = self.unwrap_obj().borrow().clone();
        if let Object::StructObj { fields, .. } = obj {
            fields
        } else {
            panic!("Expected struct, got {:?}", obj);
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    StringObj(String),
    FnObj {
        name: String,
        receiver: Option<Rc<RefCell<Object>>>,
        chunk: Chunk,
        arity: u8,
    },
    StructObj {
        name: String,
        fields: HashMap<String, Value>,
    },
}

impl Object {
    pub fn unwrap_string(self) -> String {
        if let Object::StringObj(str_) = self {
            str_
        } else {
            panic!("Expected String.");
        }
    }

    pub fn unwrap_struct(self) -> HashMap<String, Value> {
        if let Object::StructObj { fields, .. } = self {
            fields
        } else {
            panic!("Expected struct obj.");
        }
    }
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
    OpStructInit,
    OpStructGetField,
    OpStructSetField,
    OpStructMethod,
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

#[derive(Clone)]
pub struct Token {
    /// The offset in bytes in the source file where the lexeme begins
    pub offset: u32,
    /// The length of the lexeme
    pub length: u8,
    /// The kind of the token
    pub kind: TokenKind,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
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

    pub fn from_range(range: &Range<usize>, kind: TokenKind) -> Self {
        let start = range.start as u32;
        let offset = (range.end - range.start) as u8;
        Self::new(start, offset, kind)
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

#[derive(Debug)]
pub struct CompileError {
    /// The indexes in the source string that are erroneous
    pub token_range: Range<usize>,
    /// The error message
    pub message: String,
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
    ImplToken,
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

#[derive(PartialEq)]
pub enum Declaration {
    StatementDecl(Statement),
    VarDecl(String, Expression),
    StructDecl(Token, Vec<(Token, Token)>),
    FnDecl(String, Vec<(Token, Token)>, Option<Token>, Statement),
    ImplDecl {
        struct_name: Token,
        methods: Vec<Declaration>,
    },
}

#[derive(PartialEq)]
pub enum Statement {
    Print(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Ret(Option<Expression>),
    Block(Vec<Declaration>),
    ExpressionStmt(Expression),
}

#[derive(PartialEq)]
pub enum ExpressionKind {
    Binary(Box<Expression>, Token, Box<Expression>),
    Assign {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    Unary(Token, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Access {
        expr: Box<Expression>,
        name: Box<Expression>,
    },
    StructInit {
        name: Box<Expression>,
        values: Vec<(Expression, Expression)>,
    },
    Integer {
        int: i32,
    },
    Double {
        float: f32,
    },
    Str {
        string: String,
    },
    Identifier(String),
    False,
    True,
}

pub struct Expression {
    pub tokens: Range<usize>,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn new(tokens: Range<usize>, kind: ExpressionKind) -> Self {
        Expression { tokens, kind }
    }

    pub fn new_debug(kind: ExpressionKind) -> Self {
        Expression { tokens: 0..1, kind }
    }

    pub fn get_id(&self) -> String {
        if let ExpressionKind::Identifier(id) = &self.kind {
            id.clone()
        } else {
            panic!("Expected IdToken");
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
            Value::Obj(obj) => write!(f, "{}", obj.borrow().clone()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::StringObj(str_) => write!(f, "{}", str_),
            Object::FnObj {
                name,
                arity,
                receiver,
                ..
            } => {
                if let Some(recv) = receiver {
                    write!(f, "{} ({}) <{}>", name, arity, recv.borrow())
                } else {
                    write!(f, "{} ({})", name, arity)
                }
            }
            Object::StructObj { name, .. } => write!(f, "{} {{}}", name),
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
    use Bytecode::*;

    if let Some((index, byte_)) = bytes.next() {
        let byte = unsafe { std::mem::transmute::<u8, Bytecode>(*byte_) };
        let res = match byte {
            OpPop | OpMul | OpAdd | OpDiv | OpSub | OpNot | OpEqual | OpNegate | OpGreater
            | OpLess | OpGreaterEqual | OpLessEqual | OpPrint | OpStructGetField | OpCall
            | OpStructSetField => format!("{:?}", byte),
            OpConstant => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpDefineGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpSetGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpGetGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpSetLocal => {
                let index = read_u8(bytes);
                format!("{:?} {}", byte, index)
            }
            OpGetLocal => {
                let index = read_u8(bytes);
                format!("{:?} {}", byte, index)
            }
            OpJumpIfFalse => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            OpJump => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            OpLoop => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            OpStructInit => {
                let index = read_u8(bytes);
                format!("{:?} of len {}", byte, index)
            }
            OpStructMethod => {
                let index = read_u16(bytes);
                let struct_name = &constants[index as usize];
                let index = read_u16(bytes);
                let method_name = &constants[index as usize];
                format!("{:?} Add {} to {}", byte, method_name, struct_name)
            }
            OpReturn => {
                let retvals = read_u8(bytes);
                let pop = read_u8(bytes);
                format!("{:?} {} vals, pop {}", byte, retvals, pop)
            }
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
