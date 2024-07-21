use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::Range;

use crate::types::{IdentifierToken, Token};

pub const SELF: &str = "self";

#[derive(PartialEq, Clone)]
pub struct MethodSelf {
    pub type_token: Option<IdentifierToken>,
}

#[derive(PartialEq, Clone)]
pub struct MethodDeclaration {
    pub name: IdentifierToken,
    pub self_: Option<MethodSelf>,
    pub params: Vec<(IdentifierToken, IdentifierToken)>,
    pub return_ty: Option<IdentifierToken>,
    pub body: Statement,
}

#[derive(PartialEq, Clone)]
pub enum Declaration {
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
    ImplMethodDecl {
        type_name: IdentifierToken,
        trait_name: Option<IdentifierToken>,
        method: MethodDeclaration,
    },
}

#[derive(PartialEq, Clone)]
pub enum Statement {
    Print(Expression),
    LetBinding(String, Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Ret(Option<Expression>),
    Block(Vec<Statement>),
    ExpressionStmt(Expression),
}

#[derive(PartialEq, Debug, Clone)]
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

#[derive(Clone)]
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
