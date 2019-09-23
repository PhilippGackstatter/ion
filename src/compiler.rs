use crate::types::{
    Bytecode, Chunk,
    Expression::{self, *},
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

    pub fn compile_(&mut self, expr: &Expression) {
        match expr {
            Binary(lexpr, op, rexpr) => {
                self.compile_(lexpr);
                self.compile_(rexpr);
                self.binary_op(op);
            }
            Unary(op, rexpr) => {
                self.compile_(rexpr);
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

    pub fn binary_op(&mut self, token: &Token) {
        match token.kind {
            Star => self.emit_op_byte(Bytecode::OpMul),
            Slash => self.emit_op_byte(Bytecode::OpDiv),
            Minus => self.emit_op_byte(Bytecode::OpSub),
            Plus => self.emit_op_byte(Bytecode::OpAdd),
            _ => unreachable!(),
        }
    }

    pub fn unary_op(&mut self, token: &Token) {
        match token.kind {
            Bang => self.emit_op_byte(Bytecode::OpNot),
            Negate => self.emit_op_byte(Bytecode::OpNegate),
            _ => unreachable!(),
        }
    }

    pub fn emit_op_byte(&mut self, byte: Bytecode) {
        self.chunk.code.push(byte as u8);
    }

    pub fn emit_byte(&mut self, byte: u8) {
        self.chunk.code.push(byte);
    }

    pub fn add_constant(&mut self, constant: Value) -> u16 {
        // TODO: Check u16 overflow?
        self.chunk.constants.push(constant);
        (self.chunk.constants.len() - 1) as u16
    }
}
