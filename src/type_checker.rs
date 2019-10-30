use crate::types::{Declaration, Expression, ExpressionKind, Program, Statement, Token, TokenKind};
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Range;

#[derive(Debug)]
pub struct TypeError {
    /// The indexes in the source string that are erroneous
    pub token_range: Range<usize>,
    /// The error message
    pub message: String,
}

#[derive(Debug, Clone)]
struct Type {
    pub token_range: Range<usize>,
    pub kind: TypeKind,
}

impl Type {
    fn new(token_range: Range<usize>, kind: TypeKind) -> Self {
        Type { token_range, kind }
    }

    fn new_empty_range(kind: TypeKind) -> Self {
        Type {
            token_range: 0..0,
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TypeKind {
    Integer,
    Str,
    Bool,
    Void,
    Func(Function),
    Struct(Struct),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Integer => write!(f, "i32"),
            TypeKind::Str => write!(f, "str"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Struct(strct) => {
                write!(f, "{}", strct.name)?;
                write!(f, "(")?;
                for field in strct.fields.iter() {
                    write!(f, "{}: {}, ", field.0, field.1)?;
                }
                write!(f, ")")
            }
            TypeKind::Func(fun) => {
                write!(f, "{} (", fun.name)?;
                for param in fun.params.iter() {
                    write!(f, "{}, ", param)?;
                }
                write!(
                    f,
                    ") -> {}",
                    if let Some(ret_ty) = &fun.result {
                        format!("{}", ret_ty)
                    } else {
                        "void".into()
                    }
                )
            }
        }
    }
}

#[derive(Debug)]
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

pub struct TypeChecker {
    locals: Vec<Variable>,
    scope_depth: u8,

    symbol_table: HashMap<String, Type>,
}

impl TypeChecker {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut hmap: HashMap<String, Type> = HashMap::new();

        hmap.insert("str".to_owned(), Type::new_empty_range(TypeKind::Str));
        hmap.insert("bool".to_owned(), Type::new_empty_range(TypeKind::Bool));
        hmap.insert("i32".to_owned(), Type::new_empty_range(TypeKind::Integer));

        TypeChecker {
            locals: vec![],
            scope_depth: 0,
            symbol_table: hmap,
        }
    }

    pub fn check(&mut self, prog: &Program) -> Result<(), TypeError> {
        for decl in prog.iter() {
            self.build_symbol_table(decl)?;
        }
        self.print_symbol_table();
        for decl in prog.iter() {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn build_symbol_table(&mut self, decl: &Declaration) -> Result<(), TypeError> {
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
                    Type::new_empty_range(TypeKind::Func(Function {
                        name: name.clone(),
                        params,
                        result,
                    })),
                );
            }
            Declaration::StructDecl(name, token_fields) => {
                let mut fields = Vec::new();
                for (name, ty) in token_fields {
                    fields.push((name.get_id(), self.lookup_type(ty)?));
                }

                let st = Type::new(
                    name.clone().into(),
                    TypeKind::Struct(Struct {
                        name: name.get_id(),
                        fields,
                    }),
                );
                self.symbol_table.insert(name.get_id(), st);
            }
            _ => (),
        }
        Ok(())
    }

    fn lookup_type(&self, token: &Token) -> Result<Type, TypeError> {
        let type_name = token.get_id();
        if let Some(symbol) = self.symbol_table.get(&type_name) {
            let mut ty = symbol.clone();
            ty.token_range = token.clone().into();
            Ok(ty)
        } else {
            Err(TypeError {
                token_range: token.clone().into(),
                message: format!("Type {} not declared in this scope.", type_name),
            })
        }
    }

    fn check_decl(&mut self, decl: &Declaration) -> Result<Vec<Type>, TypeError> {
        match decl {
            Declaration::StatementDecl(stmt) => {
                return self.check_stmt(stmt);
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
                        return Err(TypeError {
                            token_range: expr_type.token_range.clone(),
                            message: "Variable is already declared in this scope.".into(),
                        });
                    }
                    self.add_local(id.clone(), expr_type);
                }
            }
            Declaration::FnDecl(_name, params_tokens, return_token, body) => {
                for (name_token, param_token) in params_tokens {
                    let expr_type = self.lookup_type(param_token)?;
                    // Make the parameters available as locals to the function body
                    // so that they can be found & type checked
                    self.add_local_to_next_scope(name_token.get_id(), expr_type);
                }
                let return_types = self.check_stmt(body)?;

                let declared_ret_type = if let Some(return_type) = return_token {
                    self.lookup_type(return_type)?
                } else {
                    Type::new_empty_range(TypeKind::Void)
                };

                if declared_ret_type.kind != TypeKind::Void && return_types.is_empty() {
                    return Err(TypeError {
                        token_range: declared_ret_type.token_range.clone(),
                        message: format!(
                            "This function has to return a type {}.",
                            declared_ret_type
                        ),
                    });
                }

                for return_type in return_types {
                    if return_type != declared_ret_type {
                        return Err(TypeError {
                            token_range: return_type.token_range.clone(),
                            message: format!(
                                "Returned type {} does not match declared return type {}.",
                                return_type, declared_ret_type
                            ),
                        });
                    }
                }
            }
            _ => (),
        }
        Ok(vec![])
    }

    fn check_stmt(&mut self, stmt: &Statement) -> Result<Vec<Type>, TypeError> {
        match stmt {
            Statement::ExpressionStmt(expr) => {
                self.check_expr(expr)?;
                Ok(vec![])
            }
            Statement::Block(decls) => {
                self.begin_scope();
                let mut return_types = Vec::new();
                for decl in decls.iter() {
                    return_types.extend(self.check_decl(decl)?);
                }
                self.end_scope();
                Ok(return_types)
            }
            Statement::Ret(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let expr_type = self.check_expr(expr)?;
                    Ok(vec![expr_type])
                } else {
                    Ok(vec![Type::new_empty_range(TypeKind::Void)])
                }
            }
            Statement::If(condition, if_branch, else_branch_opt) => {
                let cond_type = self.check_expr(condition)?;
                if cond_type.kind != TypeKind::Bool {
                    return Err(TypeError {
                        token_range: condition.tokens.clone(),
                        message: format!("Condition must be of type bool, got {}", cond_type),
                    });
                }

                let mut return_types = self.check_stmt(if_branch)?;

                if let Some(else_branch) = else_branch_opt {
                    return_types.extend(self.check_stmt(else_branch)?);
                }

                Ok(return_types)
            }
            _ => Ok(vec![]),
        }
    }

    fn check_expr(&self, expr: &Expression) -> Result<Type, TypeError> {
        match &expr.kind {
            ExpressionKind::Binary(lexpr, op_token, rexpr) => {
                let ltype = self.check_expr(&lexpr)?;
                let rtype = self.check_expr(&rexpr)?;
                if ltype == rtype {
                    if [
                        TokenKind::EqualEqual,
                        TokenKind::Greater,
                        TokenKind::Less,
                        TokenKind::LessEqual,
                        TokenKind::GreaterEqual,
                    ]
                    .contains(&op_token.kind)
                    {
                        Ok(Type::new(
                            ltype.token_range.start..rtype.token_range.end,
                            TypeKind::Bool,
                        ))
                    } else {
                        Ok(Type::new(
                            ltype.token_range.start..rtype.token_range.end,
                            rtype.kind,
                        ))
                    }
                } else {
                    Err(TypeError {
                        token_range: op_token.clone().into(),
                        message: format!(
                            "Types {} and {} are not compatible in binary operation",
                            ltype, rtype
                        ),
                    })
                }
            }
            ExpressionKind::Unary(op, rexpr) => {
                let expr_type = self.check_expr(&rexpr)?;
                match op.kind {
                    TokenKind::Bang => {
                        if expr_type.kind == TypeKind::Bool {
                            Ok(Type::new(expr_type.token_range.clone(), TypeKind::Bool))
                        } else {
                            Err(TypeError {
                                token_range: expr_type.token_range.clone(),
                                message: format!(
                                    "Type {} can not be used with a ! operator",
                                    expr_type
                                ),
                            })
                        }
                    }
                    TokenKind::Minus => {
                        if expr_type.kind == TypeKind::Integer {
                            Ok(Type::new(expr_type.token_range.clone(), TypeKind::Integer))
                        } else {
                            Err(TypeError {
                                token_range: expr_type.token_range.clone(),
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
            ExpressionKind::Integer { .. } => Ok(Type::new(expr.tokens.clone(), TypeKind::Integer)),
            ExpressionKind::Str { .. } => Ok(Type::new(expr.tokens.clone(), TypeKind::Str)),
            ExpressionKind::True { .. } => Ok(Type::new(expr.tokens.clone(), TypeKind::Bool)),
            ExpressionKind::False { .. } => Ok(Type::new(expr.tokens.clone(), TypeKind::Bool)),
            ExpressionKind::Assign(id, expr) => {
                let expr_type = self.check_expr(&expr)?;

                if let Some(index) = self.find_local_variable(&id) {
                    // Assignment to local var
                    if self.locals[index as usize].dtype != expr_type {
                        Err(TypeError {
                            token_range: expr_type.token_range.clone(),
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
            ExpressionKind::Identifier(id) => {
                if let Some(index) = self.find_local_variable(&id) {
                    Ok(self.locals[index as usize].dtype.clone())
                } else if let Some(ty) = self.symbol_table.get(id) {
                    Ok(ty.clone())
                } else {
                    panic!("Globals unimplemented, looking for {}", id);
                }
            }
            ExpressionKind::Call(callee, params) => {
                let callee_type = self.check_expr(&callee)?;
                if let TypeKind::Func(fun) = callee_type.kind {
                    for (index, param) in fun.params.iter().enumerate() {
                        if let Some(call_param) = params.get(index) {
                            let call_param_type = self.check_expr(call_param)?;
                            if *param != call_param_type {
                                return Err(TypeError {
                                    token_range: call_param_type.token_range.clone(),
                                    message: format!(
                                        "Function parameters have incompatible type. Expected: {}, Supplied: {}.",
                                        param,
                                        call_param_type
                                    ),
                                });
                            }
                        } else {
                            return Err(TypeError {
                                token_range: callee_type.token_range.clone(),
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
                        Type::new_empty_range(TypeKind::Void)
                    })
                } else {
                    Err(TypeError {
                        token_range: callee_type.token_range.clone(),
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
        let mut i = self.locals.len();
        while i > 0 {
            i -= 1;

            // Since we iterate backwards, once we hit a variable
            // that's not in the current scope, we've popped all
            // the locals in this scope.
            if self.locals[i].scope_depth != self.scope_depth {
                break;
            }
            self.locals.pop().unwrap();
        }

        self.scope_depth -= 1;
    }

    fn add_local(&mut self, name: String, dtype: Type) {
        self.locals
            .push(Variable::new(name, self.scope_depth, dtype));
    }

    fn add_local_to_next_scope(&mut self, name: String, dtype: Type) {
        self.scope_depth += 1;
        self.add_local(name, dtype);
        self.scope_depth -= 1;
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

    fn print_symbol_table(&self) {
        println!("\nSymbol Table");
        println!("============");
        for (_name, symbol) in self.symbol_table.iter() {
            println!("{}", symbol);
        }
    }
}
