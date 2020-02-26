use crate::types::{
    CompileError, Declaration, Expression, ExpressionKind, Program, Statement, Token, TokenKind,
};
use std::collections::{hash_map::Entry, HashMap};
use std::convert::TryInto;
use std::ops::Range;

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
    number_of_fields: usize,
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

    pub fn check(&mut self, prog: &Program, print_symbol_table: bool) -> Result<(), CompileError> {
        for decl in prog.iter() {
            self.build_symbol_table(decl)?;
        }
        if print_symbol_table {
            self.print_symbol_table();
        }
        for decl in prog.iter() {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn add_symbol(&mut self, name: &str, ty: Type) -> Result<(), CompileError> {
        if let entry @ Entry::Vacant(_) = self.symbol_table.entry(name.into()) {
            entry.or_insert(ty);
            Ok(())
        } else {
            Err(CompileError {
                token_range: ty.token_range.clone(),
                message: format!("Type {} is already declared in this scope.", name),
            })
        }
    }

    fn build_symbol_table(&mut self, decl: &Declaration) -> Result<(), CompileError> {
        match decl {
            Declaration::FnDecl(name, params_tokens, return_token, _stmt) => {
                self.add_symbol(
                    name,
                    self.generate_function_type(name, params_tokens, return_token, _stmt)?,
                )?;
            }
            Declaration::StructDecl(name, token_fields) => {
                let mut fields = Vec::new();
                for (name, ty) in token_fields {
                    fields.push((name.get_id(), self.lookup_type(ty)?));
                }

                let number_of_fields = fields.len();
                let st = Type::new(
                    name.clone().into(),
                    TypeKind::Struct(Struct {
                        name: name.get_id(),
                        fields,
                        number_of_fields,
                    }),
                );
                self.add_symbol(&name.get_id(), st)?;
            }
            _ => (),
        }
        Ok(())
    }

    fn lookup_type(&self, token: &Token) -> Result<Type, CompileError> {
        let type_name = token.get_id();
        if let Some(symbol) = self.symbol_table.get(&type_name) {
            let mut ty = symbol.clone();
            ty.token_range = token.clone().into();
            Ok(ty)
        } else {
            Err(CompileError {
                token_range: token.clone().into(),
                message: format!("Type {} not declared in this scope.", type_name),
            })
        }
    }

    fn add_struct_method(
        &mut self,
        struct_name: &Token,
        method_name: &str,
        method_ty: Type,
    ) -> Result<(), CompileError> {
        let type_name = struct_name.get_id();
        if let Some(symbol) = self.symbol_table.get_mut(&type_name) {
            if let TypeKind::Struct(strct) = &mut symbol.kind {
                strct.fields.push((method_name.to_owned(), method_ty));
                Ok(())
            } else {
                Err(CompileError {
                    token_range: struct_name.clone().into(),
                    message: format!(
                        "Can only add methods to type struct, found {} instead",
                        type_name
                    ),
                })
            }
        } else {
            Err(CompileError {
                token_range: struct_name.clone().into(),
                message: format!("Type {} not declared in this scope.", type_name),
            })
        }
    }

    fn check_decl(&mut self, decl: &Declaration) -> Result<Vec<Type>, CompileError> {
        match decl {
            Declaration::StatementDecl(stmt) => {
                return self.check_stmt(stmt);
            }
            Declaration::VarDecl(id, expr) => {
                let expr_type = self.check_expr(expr)?;
                // if self.scope_depth == 0 {
                //     // Global
                // } else {
                if self
                    .locals
                    .iter()
                    .any(|elem| elem.scope_depth == self.scope_depth && elem.identifier == *id)
                {
                    return Err(CompileError {
                        token_range: expr_type.token_range.clone(),
                        message: "Variable is already declared in this scope.".into(),
                    });
                }
                self.add_local(id.clone(), expr_type);
                // }
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
                    return Err(CompileError {
                        token_range: declared_ret_type.token_range.clone(),
                        message: format!(
                            "This function has to return a type {}.",
                            declared_ret_type
                        ),
                    });
                }

                for return_type in return_types {
                    if return_type != declared_ret_type {
                        return Err(CompileError {
                            token_range: return_type.token_range.clone(),
                            message: format!(
                                "Returned type {} does not match declared return type {}.",
                                return_type, declared_ret_type
                            ),
                        });
                    }
                }
            }
            Declaration::StructDecl(_name, fields) => {
                for field in fields.iter() {
                    self.lookup_type(&field.1)?;
                }
            }
            Declaration::ImplDecl {
                struct_name,
                methods,
            } => {
                for method in methods {
                    if let Declaration::FnDecl(name, params_tokens, return_token, _stmt) = method {
                        let fn_type =
                            self.generate_function_type(name, params_tokens, return_token, _stmt)?;
                        self.add_struct_method(struct_name, name, fn_type)?;
                    }
                }
            }
        }
        Ok(vec![])
    }

    fn check_stmt(&mut self, stmt: &Statement) -> Result<Vec<Type>, CompileError> {
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
                    return Err(CompileError {
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
            Statement::While(condition, body) => {
                let cond_type = self.check_expr(condition)?;
                if cond_type.kind != TypeKind::Bool {
                    return Err(CompileError {
                        token_range: condition.tokens.clone(),
                        message: format!("Condition must be of type bool, got {}", cond_type),
                    });
                }

                self.check_stmt(body)?;

                Ok(vec![])
            }
            Statement::Print(expr) => {
                self.check_expr(expr)?;
                Ok(vec![])
            }
        }
    }

    fn check_expr(&self, expr: &Expression) -> Result<Type, CompileError> {
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
                        Ok(Type::new(expr.tokens.clone(), TypeKind::Bool))
                    } else {
                        Ok(Type::new(expr.tokens.clone(), rtype.kind))
                    }
                } else {
                    Err(CompileError {
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
                            Err(CompileError {
                                token_range: rexpr.tokens.clone(),
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
                            Err(CompileError {
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
            ExpressionKind::Assign { target, value } => {
                let value_type = self.check_expr(&value)?;
                let target_type = self.check_expr(target)?;
                if target_type != value_type {
                    Err(CompileError {
                        token_range: value_type.token_range.clone(),
                        message: format!(
                            "Expression of type {} can not be assigned to variable of type {}",
                            value_type, target_type
                        ),
                    })
                } else {
                    Ok(target_type)
                }
            }
            ExpressionKind::Identifier(id) => {
                if let Some(index) = self.find_local_variable(&id) {
                    Ok(self.locals[index as usize].dtype.clone())
                } else if let Some(ty) = self.symbol_table.get(id) {
                    Ok(ty.clone())
                } else {
                    Err(CompileError {
                        token_range: expr.tokens.clone(),
                        message: format!(
                            "{} is not defined in the current scope. (Globals are unimplemented.)",
                            id
                        ),
                    })
                }
            }
            ExpressionKind::Call(callee, params) => {
                let callee_type = self.check_expr(&callee)?;
                if let TypeKind::Func(fun) = callee_type.kind {
                    for (index, param) in fun.params.iter().enumerate() {
                        if let Some(call_param) = params.get(index) {
                            let call_param_type = self.check_expr(call_param)?;
                            if *param != call_param_type {
                                return Err(CompileError {
                                    token_range: call_param_type.token_range.clone(),
                                    message: format!(
                                        "Function parameters have incompatible type. Expected: {}, Supplied: {}.",
                                        param,
                                        call_param_type
                                    ),
                                });
                            }
                        } else {
                            return Err(CompileError {
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
                    Err(CompileError {
                        token_range: callee_type.token_range.clone(),
                        message: "Cannot call anything other than a function.".to_owned(),
                    })
                }
            }
            ExpressionKind::StructInit { name, values } => {
                let lookup_token =
                    Token::from_range(&expr.tokens, TokenKind::IdToken(name.get_id()));
                let struct_type = self.lookup_type(&lookup_token)?;
                if let TypeKind::Struct(declared_struct) = &struct_type.kind {
                    let declared_field_number = declared_struct.number_of_fields;
                    let given_field_number = values.len();
                    if declared_field_number != given_field_number {
                        Err(CompileError {
                            token_range: expr.tokens.clone(),
                            message: format!(
                                "Expected {} fields, but {} were given.",
                                declared_field_number, given_field_number
                            ),
                        })
                    } else {
                        #[allow(clippy::needless_range_loop)]
                        for i in 0..declared_field_number {
                            let declared_name = &declared_struct.fields[i].0;
                            let declared_type = &declared_struct.fields[i].1;
                            let given_name = &values[i].0;
                            let given_type = &values[i].1;

                            if *declared_name != given_name.get_id() {
                                return Err(CompileError {
                                    token_range: given_name.tokens.clone(),
                                    message: format!(
                                        "{} has no field with name {}",
                                        declared_struct.name,
                                        given_name.get_id()
                                    ),
                                });
                            }

                            let given_type = self.check_expr(given_type)?;
                            if *declared_type != given_type {
                                return Err(CompileError {
                                    token_range: given_type.token_range.clone(),
                                    message: format!(
                                        "Expected {} for field {} but type {} was found",
                                        declared_type, declared_name, given_type
                                    ),
                                });
                            }
                        }

                        Ok(struct_type)
                    }
                } else {
                    Err(CompileError {
                        token_range: expr.tokens.clone(),
                        message: "Cannot instantiate anything other than a struct".to_owned(),
                    })
                }
            }
            ExpressionKind::Access { expr, name } => {
                let expr_type = self.check_expr(expr)?;

                if let TypeKind::Struct(strct) = &expr_type.kind {
                    let field_type = strct.fields.iter().find(|elem| elem.0 == name.get_id());

                    let field_type = field_type.ok_or_else(|| CompileError {
                        token_range: name.tokens.clone(),
                        message: format!(
                            "Struct {} has no field named {}",
                            strct.name,
                            name.get_id()
                        ),
                    })?;

                    Ok(field_type.1.clone())
                } else {
                    Err(CompileError {
                        token_range: expr.tokens.clone(),
                        message: "Cannot access anything other than a struct".to_owned(),
                    })
                }
            }
            _ => unimplemented!(),
        }
    }

    fn generate_function_type(
        &self,
        name: &String,
        params_tokens: &Vec<(Token, Token)>,
        return_token: &Option<Token>,
        _stmt: &Statement,
    ) -> Result<Type, CompileError> {
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

        Ok(Type::new_empty_range(TypeKind::Func(Function {
            name: name.clone(),
            params,
            result,
        })))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser, util};
    use std::path::Path;

    fn lex_parse_check(path: &str) -> Result<(), CompileError> {
        let test_path: &Path = "src/test_input/type_checker/".as_ref();
        let input = util::file_to_string(&test_path.join(&path));
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let mut parser = Parser::new(&lexer);
        let tree = parser.parse().unwrap();
        let mut checker = crate::type_checker::TypeChecker::new();
        checker.check(&tree, false)
    }

    #[test]
    fn test_no_return() {
        let res = lex_parse_check("no_return.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_multiple_returns() {
        let res = lex_parse_check("multiple_returns.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_return_type_void() {
        let res = lex_parse_check("return_type_void.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_empty_return() {
        let res = lex_parse_check("empty_return.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_fn_arg() {
        let res = lex_parse_check("fn_arg.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("Function parameters have incompatible type"));
    }

    #[test]
    fn test_if_condition() {
        let res = lex_parse_check("if_condition.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("Condition must be of type bool"));
    }

    #[test]
    fn test_local_binary_plus() {
        let res = lex_parse_check("local_binary_plus.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("Condition must be of type bool"));
    }

    #[test]
    fn test_local_binary_greater() {
        let res = lex_parse_check("local_binary_greater.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("not compatible in binary operation"));
    }

    #[test]
    fn test_local_unary_bang() {
        let res = lex_parse_check("local_unary_bang.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("can not be used with a ! operator"));
    }

    #[test]
    fn test_redeclare_struct() {
        let res = lex_parse_check("redeclare_struct.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("already declared in this scope"));
    }

    #[test]
    fn test_struct_init_too_many_fields() {
        let res = lex_parse_check("struct_init_too_many.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("Expected 2 fields, but 3 were given"));
    }

    #[test]
    fn test_struct_init_wrong_field_name() {
        let res = lex_parse_check("struct_init_wrong_field_name.io");
        assert!(res.is_err());
        assert!(res.unwrap_err().message.contains("has no field with name"));
    }

    #[test]
    fn test_struct_init_wrong_type() {
        let res = lex_parse_check("struct_init_wrong_type.io");
        assert!(res.is_err());
        assert!(res.unwrap_err().message.contains("but type i32 was found"));
    }

    #[test]
    fn test_struct_access_use() {
        let res = lex_parse_check("struct_access_use.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("Type i32 can not be used with a ! operator"));
    }

    #[test]
    fn test_struct_access_assign() {
        let res = lex_parse_check("struct_access_assign.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .message
            .contains("Expression of type str can not be assigned to variable of type i32"));
    }
}
