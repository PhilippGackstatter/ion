use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

use crate::types::{
    Bytecode, Chunk,
    Declaration::{self, *},
    Expression,
    ExpressionKind::*,
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

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionType {
    Script,
    Function,
    Method,
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
            locals: if fn_type == FunctionType::Method {
                vec![("self".to_owned(), 1)]
            } else {
                vec![]
            },
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
                    let index = self.add_constant(make_string_value(id));
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
                self.compile_fn_decl(name, params, stmt, FunctionType::Function);

                // Declare the fn obj on the stack as a global variable associated with its name
                let index_name = self.add_constant(make_string_value(name));
                self.emit_op_byte(Bytecode::OpDefineGlobal);
                self.emit_u16(index_name);
            }
            MethodDecl {
                name,
                self_,
                params,
                return_ty: ret,
                body,
            } => {
                todo!()
            }
            // TODO: This should be compiled in a first pass, otherwise a StructInit that comes before an impl block in the source
            // code, will be created without the methods from that later impl block!
            ImplDecl {
                struct_name,
                methods,
            } => {
                for method in methods {
                    if let FnDecl(name, params, _, stmt) = method {
                        let method_name = name;

                        // Puts the compiled Function Object on the stack
                        self.compile_fn_decl(method_name, params, stmt, FunctionType::Method);

                        // Associate the Function Object with the method name on the struct
                        self.emit_op_byte(Bytecode::OpStructMethod);

                        let struct_name_index =
                            self.add_constant(make_string_value(&struct_name.get_id()));
                        self.emit_u16(struct_name_index);

                        let method_name_index = self.add_constant(make_string_value(name));
                        self.emit_u16(method_name_index);
                    }
                }
            }
            StructDecl(_, _) => (),
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
        match &expr.kind {
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
                self.emit_op_byte(Bytecode::OpConstant);
                let index = self.add_constant(make_string_value("<SELF>"));
                self.emit_u16(index);
                // Put the arguments on the stack, s.t. they're available as locals for the callee
                for param in params {
                    self.compile_expr(param);
                }

                self.compile_expr(callee);

                self.emit_op_byte(Bytecode::OpCall);
            }
            Assign { target, value } => {
                self.compile_expr(value);

                if let Identifier(ident) = &target.kind {
                    if let Some(index) = self.find_local_variable(ident) {
                        self.emit_op_byte(Bytecode::OpSetLocal);
                        self.emit_byte(index);
                    } else {
                        panic!("No local variable with name {}", ident);
                    }
                } else if let Access { expr, name } = &target.kind {
                    self.compile_expr(expr);

                    if let Identifier(ident) = &name.kind {
                        let index = self.add_constant(make_string_value(ident));
                        self.emit_op_byte(Bytecode::OpConstant);
                        self.emit_u16(index);
                    } else {
                        panic!("Expected property to be an identifier.");
                    }

                    self.emit_op_byte(Bytecode::OpStructSetField);
                }
            }
            Integer { int, .. } => {
                let index = self.add_constant(Value::Int(*int));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            Double { float } => {
                let index = self.add_constant(Value::Double(*float));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            Str { string } => {
                let index = self.add_constant(make_string_value(string));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);
            }
            True { .. } => {
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
                if let Some(index) = self.find_local_variable(id) {
                    self.emit_op_byte(Bytecode::OpGetLocal);
                    self.emit_byte(index);
                } else {
                    let index = self.add_constant(make_string_value(id));
                    self.emit_op_byte(Bytecode::OpGetGlobal);
                    self.emit_u16(index);
                }
            }
            Self_ => {
                todo!()
            }
            StructInit { name, values } => {
                for (field_name, field_value) in values.iter() {
                    self.compile_expr(field_value);
                    let index = self.add_constant(make_string_value(&field_name.get_id()));
                    self.emit_op_byte(Bytecode::OpConstant);
                    self.emit_u16(index);
                }

                let index = self.add_constant(make_string_value(&name.get_id()));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);

                self.emit_op_byte(Bytecode::OpStructInit);
                self.emit_byte(values.len().try_into().unwrap());
            }
            Access { expr, name } => {
                self.compile_expr(expr);

                let index = self.add_constant(make_string_value(&name.get_id()));
                self.emit_op_byte(Bytecode::OpConstant);
                self.emit_u16(index);

                self.emit_op_byte(Bytecode::OpStructGetField);
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

    fn compile_fn_decl(
        &mut self,
        name: &String,
        params: &Vec<(Token, Token)>,
        stmt: &Statement,
        function_type: FunctionType,
    ) {
        let mut fn_compiler = Compiler::new(function_type.clone());

        for param in params {
            fn_compiler.add_local(param.0.get_id().clone());
        }

        fn_compiler.compile_stmt(stmt);

        // Provide the final parameter to OpReturn: number of arguments to pop
        let num_locals = if function_type == FunctionType::Function {
            fn_compiler.locals.len() as u8 + 1
        } else {
            fn_compiler.locals.len() as u8
        };

        fn_compiler.emit_byte(num_locals);

        // Create the function object as constant, load it on the stack at runtime
        let fn_obj = Object::FnObj {
            name: name.clone(),
            receiver: None,
            chunk: fn_compiler.chunk().clone(),
            arity: params.len() as u8,
        };
        let index = self.add_constant(Value::Obj(Rc::new(RefCell::new(fn_obj))));

        self.emit_op_byte(Bytecode::OpConstant);
        self.emit_u16(index);
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

fn make_string_value(str_: &str) -> Value {
    Value::Obj(Rc::new(RefCell::new(Object::StringObj(str_.to_owned()))))
}
