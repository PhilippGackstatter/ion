use std::convert::TryFrom;
use std::fmt;
use std::hash::Hash;
use std::ops::Range;
use std::str::Split;

use crate::types::CompileError;

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

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    WhiteSpace(u8),
    NewLine,
    Colon,
    DoubleColon,
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
    LetToken,
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

    pub fn components(&self) -> Split<'_, &'static str> {
        self.name.split("::")
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
            _ => Err(CompileError::new_migration(
                token.range.into(),
                "Expected token to be an identifier".to_owned(),
            )),
        }
    }
}

impl PartialEq for IdentifierToken {
    fn eq(&self, other: &Self) -> bool {
        // Do not include range in comparison.
        self.name == other.name
    }
}

impl Eq for IdentifierToken {}

impl Hash for IdentifierToken {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
