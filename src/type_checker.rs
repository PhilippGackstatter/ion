use crate::types::{Declaration, Expression, Program, Statement, Token, TokenKind};
use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Debug)]
pub struct TypeCheckerError {
    pub token: Token,
    pub message: String,
}

#[derive(Debug, Clone)]
enum Type {
    Integer(usize),
    Str(usize),
    Bool(usize),
    Void,
    Func(Function),
    Struct(Struct),
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
            _ => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer(_) => write!(f, "i32"),
            Type::Str(_) => write!(f, "str"),
            Type::Bool(_) => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Struct(strct) => write!(f, "{}", strct.name),
            Type::Func(fun) => write!(f, "{}", fun.name),
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

#[derive(Debug, Clone, PartialEq)]
struct Function {
    pub name: String,
    params: Vec<Type>,
    result: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
struct Struct {
    pub name: String,
    fields: Vec<(String, Type)>,
}

pub struct TypeChecker<'a> {
    tokens: &'a Vec<Token>,

    locals: Vec<Variable>,
    scope_depth: u8,

    symbol_table: HashMap<String, Type>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        let mut hmap: HashMap<String, Type> = HashMap::new();

        hmap.insert("str".to_owned(), Type::Str(0));
        hmap.insert("bool".to_owned(), Type::Bool(0));
        hmap.insert("i32".to_owned(), Type::Integer(0));

        TypeChecker {
            tokens,
            locals: vec![],
            scope_depth: 0,
            symbol_table: hmap,
        }
    }

    pub fn check(&mut self, prog: &Program) -> Result<(), TypeCheckerError> {
        for decl in prog.iter() {
            self.build_symbol_table(decl)?;
        }
        for decl in prog.iter() {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn build_symbol_table(&mut self, decl: &Declaration) -> Result<(), TypeCheckerError> {
        match decl {
            Declaration::FnDecl(name, params_tokens, return_token, _stmt) => {
                let mut params = Vec::new();
                for (_, type_token) in params_tokens {
                    let ty = self.lookup_type(type_token)?;
                    params.push(ty);
                }
                let result = if let Some(return_ty) = return_token {
                    Some(Box::new(self.lookup_type(return_ty)?))
                } else {
                    None
                };
                self.symbol_table.insert(
                    name.clone(),
                    Type::Func(Function {
                        name: name.clone(),
                        params,
                        result,
                    }),
                );
            }
            Declaration::StructDecl(name, token_fields) => {
                let mut fields = Vec::new();
                for (name, ty) in token_fields {
                    fields.push((name.get_id(), self.lookup_type(ty)?));
                }

                let st = Type::Struct(Struct {
                    name: name.get_id(),
                    fields,
                });
                self.symbol_table.insert(name.get_id(), st);
            }
            _ => (),
        }
        Ok(())
    }

    fn lookup_type(&self, token: &Token) -> Result<Type, TypeCheckerError> {
        let type_name = token.get_id();
        if let Some(symbol) = self.symbol_table.get(&type_name) {
            Ok(symbol.clone())
        } else {
            Err(TypeCheckerError {
                token: token.clone(),
                message: format!("Type {} not declared in this scope.", type_name),
            })
        }
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

    fn check_expr(&self, expr: &Expression) -> Result<Type, TypeCheckerError> {
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
                let expr_type = self.check_expr(rexpr)?;
                match op.kind {
                    TokenKind::Bang => {
                        if expr_type == Type::Bool(0) {
                            Ok(Type::Bool(expr_type.get_token_index()))
                        } else {
                            Err(TypeCheckerError {
                                token: self.tokens[expr_type.get_token_index()].clone(),
                                message: format!(
                                    "Type {} can not be used with a ! operator",
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
                                    "Type {} can not be used with a - operator",
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
                                "Expression of type {} can not be assigned to variable with type {}",
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
            Expression::Identifier(id) => {
                if let Some(index) = self.find_local_variable(id) {
                    Ok(self.locals[index as usize].dtype.clone())
                } else {
                    // Make Type the parent of Symbol, return Symbol::Func from here
                    // then type check it in Call
                    if let Some(ty) = self.symbol_table.get(id) {
                        Ok(ty.clone())
                    } else {
                        panic!("Globals unimplemented, looking for {}", id);
                    }
                }
            }
            Expression::Call(callee, params) => {
                let callee_type = self.check_expr(callee)?;
                if let Type::Func(fun) = callee_type {
                    for (index, param) in fun.params.iter().enumerate() {
                        if let Some(call_param) = params.get(index) {
                            let call_param_type = self.check_expr(call_param)?;
                            if *param != call_param_type {
                                return Err(TypeCheckerError {
                                    token: self.tokens[call_param_type.get_token_index()].clone(),
                                    message: format!(
                                        "Function parameters have incompatible type. Expected: {}, Supplied: {}.",
                                        param,
                                        call_param_type
                                    ),
                                });
                            }
                        } else {
                            return Err(TypeCheckerError {
                                token: self.tokens[0].clone(),
                                message: format!(
                                    "Function needs {} parameters, but only {} were supplied.",
                                    fun.params.len(),
                                    params.len()
                                ),
                            });
                        }
                    }

                    Ok(if let Some(res) = &fun.result {
                        res.as_ref().clone()
                    } else {
                        Type::Void
                    })
                } else {
                    Err(TypeCheckerError {
                        token: self.tokens[0].clone(),
                        message: "Cannot call anything other than a function.".to_owned(),
                    })
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
