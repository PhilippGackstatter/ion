use crate::types::{Declaration, Expression, Program, Statement, Token, TokenKind};
use std::convert::TryInto;

#[derive(Debug)]
pub struct TypeCheckerError {
    pub token: Token,
    pub message: String,
}

#[derive(Debug)]
enum Type {
    Integer(usize),
    Str(usize),
    Bool(usize),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;

        match (self, other) {
            (&Integer(_), &Integer(_)) => true,
            (&Str(_), &Str(_)) => true,
            (&Bool(_), &Bool(_)) => true,
            _ => false,
        }
    }
}

impl Type {
    fn get_token_index(&self) -> usize {
        match self {
            Type::Integer(index) => *index,
            Type::Str(index) => *index,
            Type::Bool(index) => *index,
        }
    }
}

struct Variable {
    identifier: String,
    scope_depth: u8,
    dtype: Type,
}

impl Variable {
    fn new(identifier: String, scope_depth: u8, dtype: Type) -> Self {
        Variable {
            identifier,
            scope_depth,
            dtype,
        }
    }
}

type TypeResult = Result<Type, TypeCheckerError>;

pub struct TypeChecker<'a> {
    tokens: &'a Vec<Token>,

    locals: Vec<Variable>,
    scope_depth: u8,
}

impl<'a> TypeChecker<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        TypeChecker {
            tokens,
            locals: vec![],
            scope_depth: 0,
        }
    }

    pub fn check(&mut self, prog: &Program) -> Result<(), TypeCheckerError> {
        for decl in prog.iter() {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn check_decl(&mut self, decl: &Declaration) -> Result<(), TypeCheckerError> {
        match decl {
            Declaration::StatementDecl(stmt) => {
                self.check_stmt(stmt)?;
            }
            Declaration::VarDecl(id, expr) => {
                let expr_type = self.check_expr(expr)?;
                if self.scope_depth == 0 {
                    // Global
                } else {
                    if self
                        .locals
                        .iter()
                        .any(|elem| elem.scope_depth == self.scope_depth && elem.identifier == *id)
                    {
                        return Err(TypeCheckerError {
                            token: self.tokens[expr_type.get_token_index()].clone(),
                            message: "Variable is already declared in this scope.".into(),
                        });
                    }
                    self.add_local(id.clone(), expr_type);
                }
            }
            _ => (),
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Statement) -> Result<(), TypeCheckerError> {
        match stmt {
            Statement::ExpressionStmt(expr) => {
                self.check_expr(expr)?;
            }
            Statement::Block(decls) => {
                self.begin_scope();
                for decl in decls.iter() {
                    self.check_decl(decl)?;
                }
                self.end_scope();
            }
            _ => (),
        }
        Ok(())
    }

    fn check_expr(&self, expr: &Expression) -> TypeResult {
        match expr {
            Expression::Binary(lexpr, op_token, rexpr) => {
                let ltype = self.check_expr(lexpr)?;
                let rtype = self.check_expr(rexpr)?;
                if ltype == rtype {
                    Ok(rtype)
                } else {
                    Err(TypeCheckerError {
                        token: op_token.clone(),
                        message: format!(
                            "Types {:?} and {:?} are not compatible in binary operation",
                            ltype, rtype
                        ),
                    })
                }
            }
            Expression::Unary(op, rexpr) => {
                println!("op {:?}, expr {:?}", op, rexpr);
                let expr_type = self.check_expr(rexpr)?;
                match op.kind {
                    TokenKind::Bang => {
                        if expr_type == Type::Bool(0) {
                            Ok(Type::Bool(expr_type.get_token_index()))
                        } else {
                            Err(TypeCheckerError {
                                token: self.tokens[expr_type.get_token_index()].clone(),
                                message: format!(
                                    "Type {:?} can not be used with a ! operator",
                                    expr_type
                                ),
                            })
                        }
                    }
                    TokenKind::Minus => {
                        if expr_type == Type::Integer(0) {
                            Ok(Type::Integer(expr_type.get_token_index()))
                        } else {
                            Err(TypeCheckerError {
                                token: self.tokens[expr_type.get_token_index()].clone(),
                                message: format!(
                                    "Type {:?} can not be used with a - operator",
                                    expr_type
                                ),
                            })
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Expression::Integer(_, token_index) => Ok(Type::Integer(*token_index)),
            Expression::Str(_, token_index) => Ok(Type::Str(*token_index)),
            Expression::True(token_index) => Ok(Type::Bool(*token_index)),
            Expression::False(token_index) => Ok(Type::Bool(*token_index)),
            Expression::Assign(id, expr) => {
                let expr_type = self.check_expr(expr)?;

                if let Some(index) = self.find_local_variable(&id) {
                    // Assignment to local var
                    if self.locals[index as usize].dtype != expr_type {
                        Err(TypeCheckerError {
                            token: self.tokens[expr_type.get_token_index()].clone(),
                            message: format!(
                                "Expression of type {:?} can not be assigned to variable with type {:?}",
                                expr_type, self.locals[index as usize].dtype
                            ),
                        })
                    } else {
                        Ok(expr_type)
                    }
                } else {
                    // TODO: Assignment to global var
                    Ok(expr_type)
                }
            }
            _ => unimplemented!(),
        }
    }

    // Local Variables
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        // let mut i = self.locals.len();
        // while i > 0 {
        //     i -= 1;

        //     // Since we iterate backwards, once we hit a variable
        //     // that's not in the current scope, we've popped all
        //     // the locals in this scope.
        //     if self.locals[i].1 != self.scope_depth {
        //         break;
        //     }
        //     self.emit_op_byte(Bytecode::OpPop);
        //     self.locals.pop().unwrap();
        // }

        self.scope_depth -= 1;
    }

    fn add_local(&mut self, name: String, dtype: Type) {
        self.locals
            .push(Variable::new(name, self.scope_depth, dtype));
    }

    fn find_local_variable(&self, id: &str) -> Option<u8> {
        if let Some(pos) = self
            .locals
            .iter()
            .rev()
            .position(|elem| elem.identifier == *id)
        {
            Some((self.locals.len() - 1 - pos).try_into().unwrap())
        } else {
            None
        }
    }
}
