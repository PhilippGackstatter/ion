use crate::types::{
    Bytecode, Chunk,
    Declaration::{self, *},
    Expression::{self, *},
    Object,
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
        self.emit_op_byte(Bytecode::OpReturn);
    }

    fn compile_decl(&mut self, decl: &Declaration) {
        match decl {
            StatementDecl(stmt) => self.compile_stmt(stmt),
            _ => (),
        }
    }

    fn compile_stmt(&mut self, stmt: &Statement) {
        match stmt {
            ExpressionStmt(expr) => self.compile_expr(expr),
            If(cond, then_stmt, else_stmt_opt) => {
                self.compile_expr(cond);
                let if_jump = self.emit_jump(Bytecode::OpJumpIfFalse);
                self.compile_stmt(then_stmt);

                if let Some(else_stmt) = else_stmt_opt {
                    // If the then branch executed, we need to jump past the else branch
                    let after_else_stmt = self.emit_jump(Bytecode::OpJump);
                    // If there is an else branch, we need to jump past the jump we just inserted
                    self.patch_jump(if_jump);
                    self.compile_stmt(else_stmt);
                    self.patch_jump(after_else_stmt);
                } else {
                    self.patch_jump(if_jump);
                }
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
                let index = self.add_constant(Value::Int(*num));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_byte((index >> 8) as u8);
                self.emit_byte((index & 0xff) as u8);
            }
            _ => (),
        }
    }

    fn binary_op(&mut self, token: &Token) {
        match token.kind {
            Star => {
                self.emit_op_byte(Bytecode::OpMul);
            }
            Slash => {
                self.emit_op_byte(Bytecode::OpDiv);
            }
            Minus => {
                self.emit_op_byte(Bytecode::OpSub);
            }
            Plus => {
                self.emit_op_byte(Bytecode::OpAdd);
            }
            Greater => {
                self.emit_op_byte(Bytecode::OpGreater);
            }
            _ => unreachable!(),
        }
    }

    fn unary_op(&mut self, token: &Token) {
        match token.kind {
            Bang => {
                self.emit_op_byte(Bytecode::OpNot);
            }
            Negate => {
                self.emit_op_byte(Bytecode::OpNegate);
            }
            _ => unreachable!(),
        }
    }

    // Helpers

    /// Patches a jumps placeholder at the given index with the given jump index
    fn patch_jump(&mut self, placeholder_index: usize) {
        // We want to jump one past the last instruction
        let jump_index = self.chunk.code.len();
        self.chunk.code[placeholder_index] = (jump_index >> 8) as u8;
        self.chunk.code[placeholder_index + 1] = (jump_index & 0xff) as u8;
    }

    /// Emits the given (jump) OpCode and two u8 placeholders.
    /// Returns the first placeholders index in the bytecode.
    fn emit_jump(&mut self, byte: Bytecode) -> usize {
        self.chunk.code.push(byte as u8);
        self.chunk.code.push(0);
        self.chunk.code.push(0);
        self.chunk.code.len() - 2
    }

    /// Emits the given OpCode and returns its index in the bytecode
    fn emit_op_byte(&mut self, byte: Bytecode) -> usize {
        self.chunk.code.push(byte as u8);
        self.chunk.code.len() - 1
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
