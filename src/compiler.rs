use std::convert::TryInto;

use crate::types::{
    Bytecode, Chunk,
    Declaration::{self, *},
    Expression::{self, *},
    Object, Program,
    Statement::{self, *},
    Token,
    TokenKind::*,
    Value,
};

#[derive(Debug)]
pub struct CompilerError {
    pub token: Token,
    pub message: &'static str,
}

#[derive(PartialEq)]
pub enum FunctionType {
    Script,
    Function,
}

pub struct Compiler {
    chunk: Chunk,
    fn_type: FunctionType,

    // Local Variables
    locals: Vec<(String, u8)>,
    scope_depth: u8,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new(FunctionType::Script)
    }
}

impl Compiler {
    pub fn new(fn_type: FunctionType) -> Self {
        Compiler {
            chunk: Chunk::new(),
            locals: vec![],
            scope_depth: if fn_type == FunctionType::Script {
                0
            } else {
                1
            },
            fn_type,
        }
    }

    pub fn compile(&mut self, prog: &Program) {
        for decl in prog.iter() {
            self.compile_decl(decl);
        }
        self.emit_op_byte(Bytecode::OpReturn);
        self.emit_byte(0);
        self.emit_byte(0);
    }

    fn compile_decl(&mut self, decl: &Declaration) {
        match decl {
            StatementDecl(stmt) => self.compile_stmt(stmt),
            VarDecl(id, expr) => {
                self.compile_expr(expr);

                if self.scope_depth == 0 {
                    let index = self.add_constant(Value::Obj(Object::StringObj(id.clone())));
                    self.emit_op_byte(Bytecode::OpDefineGlobal);
                    self.emit_u16(index);
                } else {
                    if self
                        .locals
                        .iter()
                        .any(|elem| elem.1 == self.scope_depth && elem.0 == *id)
                    {
                        panic!("This variable is already declared in this scope.");
                    }
                    self.add_local(id.clone());
                }
            }
            FnDecl(name, params, _, stmt) => {
                let mut fn_compiler = Compiler::new(FunctionType::Function);

                for param in params {
                    fn_compiler.add_local(param.0.get_id().clone());
                }

                fn_compiler.compile_stmt(stmt);

                // Provide the final parameter to OpReturn: number of arguments to pop
                fn_compiler.emit_byte(fn_compiler.locals.len() as u8);

                // Create the function object as constant, load it on the stack at runtime
                let fn_obj = Object::FnObj(
                    name.clone(),
                    fn_compiler.chunk().clone(),
                    params.len() as u8,
                );
                let index = self.add_constant(Value::Obj(fn_obj));

                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);

                // Declare the fn obj on the stack as a global variable associated with its name
                let index_name = self.add_constant(Value::Obj(Object::StringObj(name.clone())));
                self.emit_op_byte(Bytecode::OpDefineGlobal);
                self.emit_u16(index_name);
            }
            StructDecl(_, _) => unimplemented!(),
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
            While(cond, body) => {
                // The beginning of the condition is our jump target
                let cond_target = self.chunk().code.len();
                self.compile_expr(cond);
                let after_stmt = self.emit_jump(Bytecode::OpJumpIfFalse);
                self.compile_stmt(body);
                self.emit_loop(Bytecode::OpLoop, cond_target as u16);
                self.patch_jump(after_stmt);
            }
            Print(expr) => {
                self.compile_expr(expr);
                self.emit_op_byte(Bytecode::OpPrint);
            }
            Ret(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.compile_expr(expr);
                    self.emit_op_byte(Bytecode::OpReturn);
                    self.emit_byte(1);
                } else {
                    self.emit_op_byte(Bytecode::OpReturn);
                    self.emit_byte(0);
                }
            }
            Block(decls) => {
                self.begin_scope();
                for decl in decls.iter() {
                    self.compile_decl(decl);
                }
                if self.fn_type != FunctionType::Function {
                    self.end_scope();
                }
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
            Call(callee, params) => {
                // Put the arguments on the stack, s.t. they're available as locals for the callee
                for param in params {
                    self.compile_expr(param);
                }

                self.compile_expr(callee);

                self.emit_op_byte(Bytecode::OpCall);
            }
            Assign(id, expr) => {
                self.compile_expr(expr);
                if let Some(index) = self.find_local_variable(&id) {
                    self.emit_op_byte(Bytecode::OpSetLocal);
                    self.emit_byte(index);
                } else {
                    let index = self.add_constant(Value::Obj(Object::StringObj(id.clone())));
                    self.emit_op_byte(Bytecode::OpSetGlobal);
                    self.emit_u16(index);
                }
            }
            Integer { int, .. } => {
                let index = self.add_constant(Value::Int(*int));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            Double(num, _) => {
                let index = self.add_constant(Value::Double(*num));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            Str { string, .. } => {
                let index = self.add_constant(Value::Obj(Object::StringObj(string.clone())));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            Expression::True { .. } => {
                let index = self.add_constant(Value::Bool(true));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            False { .. } => {
                let index = self.add_constant(Value::Bool(false));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            Identifier(id) => {
                if let Some(index) = self.find_local_variable(&id) {
                    self.emit_op_byte(Bytecode::OpGetLocal);
                    self.emit_byte(index);
                } else {
                    let index = self.add_constant(Value::Obj(Object::StringObj(id.clone())));
                    self.emit_op_byte(Bytecode::OpGetGlobal);
                    self.emit_u16(index);
                }
            }
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
            Less => {
                self.emit_op_byte(Bytecode::OpLess);
            }
            GreaterEqual => {
                self.emit_op_byte(Bytecode::OpGreaterEqual);
            }
            LessEqual => {
                self.emit_op_byte(Bytecode::OpLessEqual);
            }
            Bang => {
                self.emit_op_byte(Bytecode::OpNot);
            }
            EqualEqual => {
                self.emit_op_byte(Bytecode::OpEqual);
            }
            BangEqual => {
                self.emit_op_byte(Bytecode::OpEqual);
                self.emit_op_byte(Bytecode::OpNot);
            }
            _ => unreachable!(),
        }
    }

    fn unary_op(&mut self, token: &Token) {
        match token.kind {
            Bang => {
                self.emit_op_byte(Bytecode::OpNot);
            }
            Minus => {
                self.emit_op_byte(Bytecode::OpNegate);
            }
            _ => unreachable!(),
        }
    }

    // Helpers

    pub fn chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    /// Patches a jumps placeholder at the given index with the given jump index
    fn patch_jump(&mut self, placeholder_index: usize) {
        // We want to jump one past the last instruction
        let jump_index = self.chunk().code.len();
        self.chunk().code[placeholder_index] = (jump_index >> 8) as u8;
        self.chunk().code[placeholder_index + 1] = (jump_index & 0xff) as u8;
    }

    fn emit_u16(&mut self, short: u16) {
        self.emit_byte((short >> 8) as u8);
        self.emit_byte((short & 0xff) as u8);
    }

    /// Emits the given (jump) OpCode and two u8 placeholders.
    /// Returns the first placeholders index in the bytecode.
    fn emit_jump(&mut self, byte: Bytecode) -> usize {
        self.emit_byte(byte as u8);
        self.emit_byte(0);
        self.emit_byte(0);
        self.chunk().code.len() - 2
    }

    fn emit_loop(&mut self, jump: Bytecode, target: u16) {
        self.emit_byte(jump as u8);
        self.emit_u16(target);
    }

    /// Emits the given OpCode and returns its index in the bytecode
    fn emit_op_byte(&mut self, byte: Bytecode) -> usize {
        self.emit_byte(byte as u8);
        self.chunk().code.len() - 1
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk().code.push(byte);
    }

    fn add_constant(&mut self, constant: Value) -> u16 {
        // TODO: Check u16 overflow?
        self.chunk().constants.push(constant);
        (self.chunk().constants.len() - 1) as u16
    }

    fn find_local_variable(&mut self, id: &str) -> Option<u8> {
        if let Some(pos) = self.locals.iter().rev().position(|elem| elem.0 == *id) {
            Some((self.locals.len() - 1 - pos).try_into().unwrap())
        } else {
            None
        }
    }

    fn add_local(&mut self, name: String) {
        self.locals.push((name, self.scope_depth));
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let mut i = self.locals.len();
        while i > 0 {
            i -= 1;

            // Since we iterate backwards, once we hit a variable
            // that's not in the current scope, we've popped all
            // the locals in this scope.
            if self.locals[i].1 != self.scope_depth {
                break;
            }
            self.emit_op_byte(Bytecode::OpPop);
            self.locals.pop().unwrap();
        }

        self.scope_depth -= 1;
    }
}
