use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::convert::TryFrom;
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TokenRange {
    /// The offset in bytes in the source file where the lexeme begins
    pub offset: u32,
    /// The length of the lexeme
    pub length: u8,
}

#[derive(Clone)]
pub struct Token {
    /// The range of the token in the source code.
    pub range: TokenRange,
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
            range: TokenRange { offset, length },
            kind,
        }
    }

    pub fn new_debug(kind: TokenKind) -> Self {
        Token {
            range: TokenRange {
                offset: 0,
                length: 0,
            },
            kind,
        }
    }

    pub fn from_range(range: &Range<usize>, kind: TokenKind) -> Self {
        let start = range.start as u32;
        let offset = (range.end - range.start) as u8;
        Self::new(start, offset, kind)
    }

    pub fn is_id_token(&self) -> bool {
        matches!(self.kind, TokenKind::IdToken(_))
    }

    pub fn unwrap_id(&self) -> String {
        if let TokenKind::IdToken(id) = &self.kind {
            id.clone()
        } else {
            panic!("Expected IdToken");
        }
    }
}

// TODO: Remove after refactor.
impl From<Token> for std::ops::Range<usize> {
    fn from(token: Token) -> Self {
        let off = token.range.offset as usize;
        off..(off + (token.range.length as usize))
    }
}

impl From<TokenRange> for std::ops::Range<usize> {
    fn from(range: TokenRange) -> Self {
        let off = range.offset as usize;
        off..(off + (range.length as usize))
    }
}

impl From<Range<usize>> for TokenRange {
    fn from(range: Range<usize>) -> Self {
        let offset = range.start;
        let length = range.end - range.start;
        Self {
            offset: u32::try_from(offset).expect("offset should be convertable to u32"),
            length: u8::try_from(length).expect("range length should be convertable to u8"),
        }
    }
}

pub const SELF: &str = "self";

#[derive(Debug)]
pub struct CompileError {
    /// The indices in the source string that are erroneous
    pub token_range: Range<usize>,
    /// The error message
    pub message: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    WhiteSpace(u8),
    NewLine,
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
    TraitToken,
    StructToken,
    ImplToken,
    IdToken(String),
    PrintToken,
    SelfToken,
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

#[derive(Debug, Clone)]
pub struct IdentifierToken {
    pub name: String,
    pub range: TokenRange,
}

impl IdentifierToken {
    pub fn new(range: impl Into<TokenRange>, name: String) -> Self {
        Self {
            name,
            range: range.into(),
        }
    }

    pub fn new_debug(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            range: TokenRange {
                offset: 0,
                length: 0,
            },
        }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl AsRef<str> for IdentifierToken {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for IdentifierToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl TryFrom<Token> for IdentifierToken {
    type Error = CompileError;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token.kind {
            TokenKind::IdToken(identifier) => Ok(Self {
                name: identifier,
                range: token.range,
            }),
            _ => Err(CompileError {
                token_range: token.range.into(),
                message: "Expected token to be an identifier".to_owned(),
            }),
        }
    }
}

impl PartialEq for IdentifierToken {
    fn eq(&self, other: &Self) -> bool {
        // Do not include range in comparison.
        self.name == other.name
    }
}

#[derive(PartialEq)]
pub struct MethodSelf {
    pub type_token: Option<IdentifierToken>,
}

#[derive(PartialEq)]
pub struct MethodDeclaration {
    pub name: IdentifierToken,
    pub self_: Option<MethodSelf>,
    pub params: Vec<(IdentifierToken, IdentifierToken)>,
    pub return_ty: Option<IdentifierToken>,
    pub body: Statement,
}

#[derive(PartialEq)]
pub enum Declaration {
    StatementDecl(Statement),
    VarDecl(String, Expression),
    TraitDecl {
        trait_identifier: IdentifierToken,
        methods: Vec<MethodDeclaration>,
    },
    StructDecl {
        identifier: IdentifierToken,
        fields: Vec<(IdentifierToken, IdentifierToken)>,
    },
    FnDecl {
        identifier: IdentifierToken,
        params: Vec<(IdentifierToken, IdentifierToken)>,
        return_ty: Option<IdentifierToken>,
        body: Statement,
    },
    ImplDecl {
        struct_name: IdentifierToken,
        trait_name: Option<IdentifierToken>,
        methods: Vec<MethodDeclaration>,
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

#[derive(PartialEq, Debug)]
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

    pub fn unwrap_identifier(&self) -> String {
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

pub struct MethodHeader {
    pub name: IdentifierToken,
    pub method_self: Option<MethodSelf>,
    pub parameters: Vec<(IdentifierToken, IdentifierToken)>,
    pub return_type: Option<IdentifierToken>,
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
