use crate::types::{
    Bytecode, Chunk,
    Declaration::{self, *},
    Expression::{self, *},
    Statement::{self, *},
    Token,
    TokenKind::*,
    Value,
};

#[derive(Default)]
pub struct Compiler {
    chunk: Chunk,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            chunk: Chunk::new(),
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn compile(&mut self, decl: &Declaration) {
        self.compile_decl(decl);
    }

    fn compile_decl(&mut self, decl: &Declaration) {
        match decl {
            StatementDecl(stmt) => self.compile_stmt(stmt),
        }
    }

    fn compile_stmt(&mut self, stmt: &Statement) {
        match stmt {
            ExpressionStmt(expr) => self.compile_expr(expr),
            If(cond, then_stmt, else_stmt_opt) => {
                unimplemented!();
            }
            Print(expr) => {
                self.compile_expr(expr);
                self.emit_op_byte(Bytecode::OpPrint);
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expression) {
        match expr {
            Binary(lexpr, op, rexpr) => {
                self.compile_expr(lexpr);
                self.compile_expr(rexpr);
                self.binary_op(op);
            }
            Unary(op, rexpr) => {
                self.compile_expr(rexpr);
                self.unary_op(op);
            }
            Number(num) => {
                let index = self.add_constant(*num);
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_byte((index >> 8) as u8);
                self.emit_byte((index & 0xff) as u8);
            }
            _ => (),
        }
    }

    fn binary_op(&mut self, token: &Token) {
        match token.kind {
            Star => self.emit_op_byte(Bytecode::OpMul),
            Slash => self.emit_op_byte(Bytecode::OpDiv),
            Minus => self.emit_op_byte(Bytecode::OpSub),
            Plus => self.emit_op_byte(Bytecode::OpAdd),
            _ => unreachable!(),
        }
    }

    fn unary_op(&mut self, token: &Token) {
        match token.kind {
            Bang => self.emit_op_byte(Bytecode::OpNot),
            Negate => self.emit_op_byte(Bytecode::OpNegate),
            _ => unreachable!(),
        }
    }

    fn emit_op_byte(&mut self, byte: Bytecode) {
        self.chunk.code.push(byte as u8);
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.code.push(byte);
    }

    fn add_constant(&mut self, constant: Value) -> u16 {
        // TODO: Check u16 overflow?
        self.chunk.constants.push(constant);
        (self.chunk.constants.len() - 1) as u16
    }
}
