use crate::types::{
    CompilationErrorKind, CompileError, Declaration, Expression, ExpressionKind, Function,
    IdentifierToken, LocatedType, MethodDeclaration, MoveContext, Moved, Program, RcType,
    Statement, Struct, Token, TokenKind, TokenRange, Trait, Type, TypeKind, Variable, WeakType,
    SELF,
};
use std::cell::RefCell;
use std::collections::{hash_map::Entry, HashMap};
use std::convert::TryInto;
use std::ops::{Deref, Range};
use std::rc::Rc;

pub struct TypeChecker {
    locals: Vec<Variable>,
    scope_depth: u8,

    symbol_table: HashMap<String, RcType>,
}

fn wrap_typekind(kind: TypeKind) -> RcType {
    Rc::new(RefCell::new(Type::new(kind)))
}

fn wrap_type(typ: Type) -> RcType {
    Rc::new(RefCell::new(typ))
}

impl TypeChecker {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut hmap: HashMap<String, RcType> = HashMap::new();

        hmap.insert("str".to_owned(), wrap_typekind(TypeKind::Str));
        hmap.insert("bool".to_owned(), wrap_typekind(TypeKind::Bool));
        hmap.insert("i32".to_owned(), wrap_typekind(TypeKind::Integer));
        hmap.insert("f32".to_owned(), wrap_typekind(TypeKind::Double));

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

    fn add_symbol(
        &mut self,
        name: &str,
        located_type: LocatedType,
    ) -> Result<&RcType, CompileError> {
        if let entry @ Entry::Vacant(_) = self.symbol_table.entry(name.into()) {
            Ok(entry.or_insert(located_type.typ))
        } else {
            Err(CompileError::new_migration(
                located_type.token_range.clone(),
                format!("Type {} is already declared in this scope.", name),
            ))
        }
    }

    fn build_symbol_table(&mut self, decl: &Declaration) -> Result<(), CompileError> {
        match decl {
            Declaration::FnDecl {
                identifier,
                params,
                return_ty,
                body,
            } => {
                log::debug!("symbol fn {identifier}");
                self.add_symbol(
                    identifier.as_str(),
                    self.generate_function_type(identifier.as_str(), params, return_ty, body)?,
                )?;
            }
            Declaration::StructDecl {
                identifier,
                fields: token_fields,
            } => {
                log::debug!("symbol struct {}", identifier);

                let mut fields = Vec::new();
                for (field_name, ty) in token_fields {
                    let type_ref = Rc::downgrade(&self.lookup_type_ref(ty)?);
                    fields.push((field_name.name.clone(), type_ref));
                }

                let number_of_fields = fields.len();
                let st = LocatedType::new(
                    identifier.range.into(),
                    wrap_typekind(TypeKind::Struct(Struct {
                        name: identifier.name.clone(),
                        fields,
                        number_of_fields,
                    })),
                );
                self.add_symbol(&identifier.name, st)?;
            }
            Declaration::TraitDecl {
                trait_identifier,
                methods,
            } => {
                log::debug!("symbol trait {}", trait_identifier);

                let mut method_types = Vec::with_capacity(methods.len());
                for method in methods {
                    let MethodDeclaration {
                        name: method_name,
                        self_: _,
                        params,
                        return_ty,
                        body,
                    } = method;
                    let method_type =
                        self.generate_function_type(method_name.as_str(), params, return_ty, body)?;
                    let trait_method_name =
                        format!("{}::{}", trait_identifier.as_str(), method_name.as_str());
                    let fn_type_ref =
                        Rc::downgrade(self.add_symbol(&trait_method_name, method_type)?);
                    method_types.push((method_name.name.to_owned(), fn_type_ref));
                }

                let trait_type = LocatedType::new(
                    trait_identifier.range.into(),
                    wrap_type(
                        Type::new(TypeKind::Trait(Trait {
                            name: trait_identifier.to_string(),
                        }))
                        .with_methods(method_types),
                    ),
                );
                self.add_symbol(trait_identifier.as_str(), trait_type)?;
            }
            Declaration::ImplDecl {
                struct_name,
                trait_name,
                methods,
            } => {
                log::debug!(
                    "symbol impl block {:?}, {}",
                    trait_name,
                    struct_name.as_str()
                );

                let mut struct_method_types = HashMap::with_capacity(methods.len());

                for method in methods {
                    let MethodDeclaration {
                        name,
                        self_: _,
                        params,
                        return_ty,
                        body,
                    } = method;

                    let method_type =
                        self.generate_function_type(name.as_str(), params, return_ty, body)?;
                    struct_method_types.insert(
                        method.name.clone().to_string(),
                        (method.name.clone(), method_type),
                    );
                }

                match trait_name {
                    Some(trt) => {
                        let trait_located_type = self.lookup_type(trt)?;

                        let typ = trait_located_type.typ.borrow();
                        if let TypeKind::Trait(_) = &typ.kind {
                            for (method_name, method_type) in &typ.methods {
                                // TODO: Allow methods with default_impl to be missing!
                                let (name_token, struct_method) = struct_method_types
                                    .get(method_name.as_str())
                                    .ok_or_else(|| {
                                        let struct_range: Range<usize> = struct_name.range.into();
                                        let trait_range: Range<usize> = trt.range.into();

                                        CompileError::new_migration(
                                            // Provide better error by pointing to a larger part of the impl trait range.
                                            trait_range.start..struct_range.end,
                                            format!(
                                                "Not all trait items implemented, missing {}",
                                                method_name.as_str()
                                            ),
                                        )
                                    })?;

                                let rc_method_type = method_type.upgrade().unwrap();

                                if struct_method.typ.borrow().deref()
                                    != rc_method_type.borrow().deref()
                                {
                                    return Err(CompileError::new_migration(
                                        name_token.range.into(),
                                        format!(
                                            "Type mismatch, expected {}, found {}",
                                            rc_method_type.borrow(),
                                            struct_method.typ.borrow(),
                                        ),
                                    ));
                                }
                            }

                            // If the trait method check was successful, this struct is an implementor of the trait.
                            self.add_struct_trait_implementor(struct_name, trt)?;
                        } else {
                            todo!("compile error")
                        }
                    }
                    None => (),
                }

                for (name, (_, method_type)) in struct_method_types {
                    let struct_method_name = format!("{}::{}", struct_name, name);
                    let fn_type_ref =
                        Rc::downgrade(self.add_symbol(&struct_method_name, method_type)?);
                    self.add_method_to_type(struct_name, &name, fn_type_ref)?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn lookup_type(&self, token: &IdentifierToken) -> Result<LocatedType, CompileError> {
        let type_ref = self.lookup_type_ref(token)?;
        Ok(LocatedType::new(token.range.into(), type_ref))
    }

    fn lookup_type_ref(&self, token: &IdentifierToken) -> Result<RcType, CompileError> {
        let type_name = token.as_str();
        if let Some(symbol) = self.symbol_table.get(type_name) {
            Ok(Rc::clone(symbol))
        } else {
            Err(CompileError::new_migration(
                token.range.into(),
                format!("Type {} not declared in this scope.", type_name),
            ))
        }
    }

    fn add_method_to_type(
        &mut self,
        struct_name: &IdentifierToken,
        method_name: &str,
        method_type: WeakType,
    ) -> Result<(), CompileError> {
        let type_name = &struct_name.name;
        if let Some(symbol) = self.symbol_table.get_mut(type_name) {
            symbol
                .borrow_mut()
                .methods
                .push((method_name.to_owned(), method_type));
            Ok(())
        } else {
            Err(CompileError::new_migration(
                struct_name.range.into(),
                format!("Type {} not declared in this scope.", type_name),
            ))
        }
    }

    fn add_struct_trait_implementor(
        &mut self,
        struct_name: &IdentifierToken,
        trait_name: &IdentifierToken,
    ) -> Result<(), CompileError> {
        // If the trait method check was successful, this struct is an implementor of the trait.
        let struct_ref = self.lookup_type_ref(struct_name)?;
        let mut struct_ref_mut = struct_ref.borrow_mut();
        if let TypeKind::Struct(_) = &struct_ref_mut.deref().kind {
            let trait_type = self
                .lookup_type_ref(trait_name)
                .expect("the trait name should be some in this branch");
            let weak_trait_type = Rc::downgrade(&trait_type);
            struct_ref_mut
                .traits
                .insert(trait_name.to_string(), weak_trait_type);
        } else {
            todo!("compile error, expected typekind struct as trait implementor")
        }

        Ok(())
    }

    fn check_decl(&mut self, decl: &Declaration) -> Result<Vec<LocatedType>, CompileError> {
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
                    return Err(CompileError::new_migration(
                        expr_type.token_range.clone(),
                        "Variable is already declared in this scope.".into(),
                    ));
                }

                self.move_variable(&expr.kind, id.to_owned(), expr.tokens.clone(), None);

                self.add_local(id.clone(), expr_type);
                // }
            }
            Declaration::FnDecl {
                identifier: _,
                params,
                return_ty,
                body,
            } => {
                let return_types = self.check_function_body(params, body)?;
                self.check_function_return_types(return_types, return_ty)?;
            }
            Declaration::TraitDecl {
                trait_identifier: _,
                methods: _,
            } => {}
            Declaration::StructDecl {
                identifier: _,
                fields,
            } => {
                for field in fields.iter() {
                    self.lookup_type(&field.1)?;
                }
            }
            Declaration::ImplDecl {
                struct_name,
                trait_name: _,
                methods,
            } => {
                for method in methods {
                    let MethodDeclaration {
                        name,
                        self_,
                        params,
                        return_ty,
                        body,
                    } = method;

                    match self_ {
                        Some(method_self) => {
                            match &method_self.type_token {
                                Some(type_token) => {
                                    if type_token != struct_name {
                                        let specified_type = self.lookup_type(type_token)?;
                                        return Err(CompileError::new_migration(
                                            type_token.range.into(),
                                            format!(
                                                "'self' in method {} must have type {}, got {}",
                                                name.as_str(),
                                                struct_name,
                                                specified_type,
                                            ),
                                        ));
                                    }
                                }
                                None => {}
                            }

                            let receiver_type = self.lookup_type(struct_name)?;
                            self.add_local_to_next_scope(SELF.to_owned(), receiver_type);
                        }
                        None => (),
                    }

                    let return_types = self.check_function_body(params, body)?;
                    self.check_function_return_types(return_types, return_ty)?;
                }
            }
        }
        Ok(vec![])
    }

    fn check_stmt(&mut self, stmt: &Statement) -> Result<Vec<LocatedType>, CompileError> {
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
                    Ok(vec![LocatedType::new_empty_range(wrap_typekind(
                        TypeKind::Void,
                    ))])
                }
            }
            Statement::If(condition, if_branch, else_branch_opt) => {
                let cond_type = self.check_expr(condition)?;

                if cond_type.typ.borrow().kind != TypeKind::Bool {
                    return Err(CompileError::new_migration(
                        condition.tokens.clone(),
                        format!("Condition must be of type bool, got {}", cond_type),
                    ));
                }

                let mut return_types = self.check_stmt(if_branch)?;

                if let Some(else_branch) = else_branch_opt {
                    return_types.extend(self.check_stmt(else_branch)?);
                }

                Ok(return_types)
            }
            Statement::While(condition, body) => {
                let cond_type = self.check_expr(condition)?;
                if cond_type.typ.borrow().kind != TypeKind::Bool {
                    return Err(CompileError::new_migration(
                        condition.tokens.clone(),
                        format!("Condition must be of type bool, got {}", cond_type),
                    ));
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

    fn check_expr(&mut self, expr: &Expression) -> Result<LocatedType, CompileError> {
        match &expr.kind {
            ExpressionKind::Binary(lexpr, op_token, rexpr) => {
                let ltype = self.check_expr(lexpr)?;
                let rtype = self.check_expr(rexpr)?;
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
                        Ok(LocatedType::new(
                            expr.tokens.clone(),
                            wrap_typekind(TypeKind::Bool),
                        ))
                    } else {
                        Ok(LocatedType::new(expr.tokens.clone(), rtype.typ))
                    }
                } else {
                    Err(CompileError::new_migration(
                        op_token.clone().into(),
                        format!(
                            "Types {} and {} are not compatible in binary operation",
                            ltype, rtype
                        ),
                    ))
                }
            }
            ExpressionKind::Unary(op, rexpr) => {
                let expr_type = self.check_expr(rexpr)?;
                match op.kind {
                    TokenKind::Bang => {
                        if expr_type.typ.borrow().kind == TypeKind::Bool {
                            Ok(LocatedType::new(
                                expr_type.token_range.clone(),
                                wrap_typekind(TypeKind::Bool),
                            ))
                        } else {
                            Err(CompileError::new_migration(
                                rexpr.tokens.clone(),
                                format!("Type {} can not be used with a ! operator", expr_type),
                            ))
                        }
                    }
                    TokenKind::Minus => {
                        if expr_type.typ.borrow().kind == TypeKind::Integer {
                            Ok(LocatedType::new(
                                expr_type.token_range.clone(),
                                wrap_typekind(TypeKind::Integer),
                            ))
                        } else {
                            Err(CompileError::new_migration(
                                expr_type.token_range.clone(),
                                format!("Type {} can not be used with a - operator", expr_type),
                            ))
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ExpressionKind::Integer { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                wrap_typekind(TypeKind::Integer),
            )),
            ExpressionKind::Double { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                wrap_typekind(TypeKind::Double),
            )),
            ExpressionKind::Str { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                wrap_typekind(TypeKind::Str),
            )),
            ExpressionKind::True { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                wrap_typekind(TypeKind::Bool),
            )),
            ExpressionKind::False { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                wrap_typekind(TypeKind::Bool),
            )),
            ExpressionKind::Assign { target, value } => {
                // Lookup type manually to avoid the move check in check_expr. A better solution would be welcome.
                let (assignment_identifier, target_type): (String, LocatedType) = match &target.kind
                {
                    ExpressionKind::Identifier(id) => {
                        if let Some((_, index)) = self.find_local_variable(id) {
                            (id.to_owned(), self.locals[index as usize].dtype.clone())
                        } else {
                            return Err(CompileError::new_migration(
                             expr.tokens.clone(),
                             format!(
                                "{} is not defined in the current scope. (Globals are unimplemented.)",
                                id
                            ),
                            ));
                        }
                    }
                    ExpressionKind::Access { name, .. } => {
                        let target_type = self.check_expr(target)?;
                        (name.unwrap_identifier(), target_type)
                    }
                    other => {
                        panic!(
                            "parser should only allow identifier or struct access as assignment target, received {:?}",
                            other
                        )
                    }
                };

                let value_type = self.check_expr(value)?;
                if target_type != value_type {
                    return Err(CompileError::new_migration(
                        value_type.token_range.clone(),
                        format!(
                            "Expression of type {} can not be assigned to variable of type {}",
                            value_type, target_type
                        ),
                    ));
                }

                // Even if the variable that was assigned to was previously moved,
                // assigning means it contains a new value and is thus considered unmoved.
                self.clear_moved(&assignment_identifier);
                self.move_variable(
                    &value.kind,
                    assignment_identifier,
                    value.tokens.clone(),
                    None,
                );

                Ok(target_type)
            }
            ExpressionKind::Identifier(id) => {
                if let Some((variable, index)) = self.find_local_variable(id) {
                    if let Some(move_context) = variable.move_context.as_ref() {
                        // TODO: Use move_context.token_range.into to point to the place where the variable was moved.
                        let (moved_into, moved_at) = match move_context {
                            MoveContext::Basic(basic) => (basic.moved_into.clone(), basic.moved_at),
                            MoveContext::Struct(struct_ctx) => {
                                let reason = struct_ctx.find_move_reason(id).expect("if a struct was partially moved, then there must be at least one field that caused the partial move");
                                (reason.moved_into, reason.moved_at)
                            }
                        };

                        return Err(CompileError::new(CompilationErrorKind::Moved(Moved {
                            moved_identifier: id.to_owned(),
                            moved_at,
                            error_location: expr.tokens.clone().into(),
                            moved_into,
                        })));
                    }

                    Ok(self.locals[index as usize].dtype.clone())
                } else if let Some(ty) = self.symbol_table.get(id) {
                    Ok(LocatedType::new_empty_range(ty.clone()))
                } else if id == SELF {
                    Err(CompileError::new_migration(
                        expr.tokens.clone(),
                        "method does not have 'self' as a receiver.".to_string(),
                    ))
                } else {
                    Err(CompileError::new_migration(
                        expr.tokens.clone(),
                        format!(
                            "{} is not defined in the current scope. (Globals are unimplemented.)",
                            id
                        ),
                    ))
                }
            }
            ExpressionKind::Call(callee, params) => {
                let callee_located_type = self.check_expr(callee)?;
                let callee_typ = &*callee_located_type.typ.borrow();
                if let TypeKind::Func(function) = &callee_typ.kind {
                    for (index, param) in function.params.iter().enumerate() {
                        if let Some(call_param) = params.get(index) {
                            let call_param_type = self.check_expr(call_param)?;

                            let expected_param_typekind = param.upgrade().unwrap();
                            let expected_param_type = &*expected_param_typekind.borrow();

                            self.type_usage_as(&call_param_type, expected_param_type)?;
                            self.move_variable(
                                &call_param.kind,
                                function.name.clone(),
                                call_param.tokens.clone(),
                                None,
                            );
                        } else {
                            return Err(CompileError::new_migration(
                                callee_located_type.token_range.clone(),
                                format!(
                                    "{} needs {} parameters, but only {} were supplied.",
                                    function.name,
                                    function.params.len(),
                                    params.len()
                                ),
                            ));
                        }
                    }

                    Ok(if let Some(res) = &function.result {
                        LocatedType::new_empty_range(res.upgrade().unwrap().clone())
                    } else {
                        LocatedType::new_empty_range(wrap_typekind(TypeKind::Void))
                    })
                } else {
                    Err(CompileError::new_migration(
                        callee_located_type.token_range.clone(),
                        format!("Type {} is not callable.", callee_typ),
                    ))
                }
            }
            ExpressionKind::StructInit { name, values } => {
                let lookup_token =
                    IdentifierToken::new(expr.tokens.clone(), name.unwrap_identifier());
                Token::from_range(&expr.tokens, TokenKind::IdToken(name.unwrap_identifier()));

                let struct_type = self.lookup_type(&lookup_token)?;

                if let TypeKind::Struct(declared_struct) = &struct_type.typ.borrow().kind {
                    let declared_field_number = declared_struct.number_of_fields;
                    let given_field_number = values.len();
                    if declared_field_number != given_field_number {
                        return Err(CompileError::new_migration(
                            expr.tokens.clone(),
                            format!(
                                "Expected {} fields, but {} were given.",
                                declared_field_number, given_field_number
                            ),
                        ));
                    } else {
                        #[allow(clippy::needless_range_loop)]
                        for i in 0..declared_field_number {
                            let declared_name = &declared_struct.fields[i].0;
                            let declared_type = &declared_struct.fields[i].1;
                            let given_name = &values[i].0;
                            let given_expr = &values[i].1;

                            if *declared_name != given_name.unwrap_identifier() {
                                return Err(CompileError::new_migration(
                                    given_name.tokens.clone(),
                                    format!(
                                        "{} has no field with name {}",
                                        declared_struct.name,
                                        given_name.unwrap_identifier()
                                    ),
                                ));
                            }

                            let given_type = self.check_expr(given_expr)?;
                            let rc_declared_type = declared_type.upgrade().unwrap();
                            let declared_type = &*rc_declared_type.borrow();

                            if declared_type != &*given_type.typ.borrow() {
                                return Err(CompileError::new_migration(
                                    given_type.token_range.clone(),
                                    format!(
                                        "Expected {} for field {} but type {} was found",
                                        declared_type, declared_name, given_type
                                    ),
                                ));
                            }

                            self.move_variable(
                                &given_expr.kind,
                                declared_name.to_owned(),
                                given_expr.tokens.clone(),
                                None,
                            );
                        }
                    }
                } else {
                    return Err(CompileError::new_migration(
                        expr.tokens.clone(),
                        "Cannot instantiate anything other than a struct".to_owned(),
                    ));
                }
                Ok(struct_type)
            }
            ExpressionKind::Access { expr, name } => {
                let expr_located_type = self.check_expr(expr)?;
                let expr_type = &*expr_located_type.typ.borrow();

                if let TypeKind::Struct(strct) = &expr_type.kind {
                    let field_type = strct
                        .fields
                        .iter()
                        .chain(expr_type.methods.iter())
                        .find(|elem| elem.0 == name.unwrap_identifier());

                    let field_type = field_type.ok_or_else(|| {
                        CompileError::new_migration(
                            name.tokens.clone(),
                            format!(
                                "Struct {} has no field named {}",
                                strct.name,
                                name.unwrap_identifier()
                            ),
                        )
                    })?;

                    Ok(LocatedType::new_empty_range(
                        field_type.1.upgrade().unwrap().clone(),
                    ))
                } else if let TypeKind::Trait(trt) = &expr_type.kind {
                    let method_type = expr_type
                        .methods
                        .iter()
                        .find(|elem| elem.0.as_str() == name.unwrap_identifier());

                    let method_type = method_type.ok_or_else(|| {
                        CompileError::new_migration(
                            name.tokens.clone(),
                            format!(
                                "Trait {} has no method named `{}`",
                                trt.name,
                                name.unwrap_identifier()
                            ),
                        )
                    })?;

                    Ok(LocatedType::new_empty_range(
                        method_type.1.upgrade().unwrap().clone(),
                    ))
                } else {
                    Err(CompileError::new_migration(
                        expr.tokens.clone(),
                        "Cannot access anything other than a struct".to_owned(),
                    ))
                }
            }
        }
    }

    fn check_function_body(
        &mut self,
        params: &Vec<(IdentifierToken, IdentifierToken)>,
        body: &Statement,
    ) -> Result<Vec<LocatedType>, CompileError> {
        for (name_token, param_token) in params {
            let expr_type = self.lookup_type(param_token)?;
            // Make the parameters available as locals to the function body
            // so that they can be found & type checked
            self.add_local_to_next_scope(name_token.name.clone(), expr_type);
        }

        let return_types = self.check_stmt(body)?;
        Ok(return_types)
    }

    fn check_function_return_types(
        &self,
        return_types: Vec<LocatedType>,
        return_token: &Option<IdentifierToken>,
    ) -> Result<(), CompileError> {
        let declared_ret_type = if let Some(return_type) = return_token {
            self.lookup_type(return_type)?
        } else {
            LocatedType::new_empty_range(wrap_typekind(TypeKind::Void))
        };

        if declared_ret_type.typ.borrow().kind != TypeKind::Void && return_types.is_empty() {
            return Err(CompileError::new_migration(
                declared_ret_type.token_range.clone(),
                format!("This function has to return a type {}.", declared_ret_type),
            ));
        }

        for return_type in return_types {
            if return_type != declared_ret_type {
                return Err(CompileError::new_migration(
                    return_type.token_range.clone(),
                    format!(
                        "Returned type {} does not match declared return type {}.",
                        return_type, declared_ret_type
                    ),
                ));
            }
        }
        Ok(())
    }

    fn move_variable(
        &mut self,
        moved_variable: &ExpressionKind,
        moved_into: String,
        moved_at: impl Into<TokenRange>,
        struct_move_path: Option<Vec<String>>,
    ) {
        match &moved_variable {
            ExpressionKind::Access { expr, name } => {
                let ExpressionKind::Identifier(id) = &name.kind else {
                    unreachable!("field access must be of type identifier");
                };

                let struct_move_path = match struct_move_path {
                    Some(mut path) => {
                        path.push(id.to_owned());
                        path
                    }
                    None => vec![id.to_owned()],
                };

                self.move_variable(&expr.kind, moved_into, moved_at, Some(struct_move_path));
            }
            ExpressionKind::Identifier(expression_id) => {
                let variable = self
                    .find_local_variable_mut(expression_id)
                    .expect("type checking of the expression must have suceeded before");

                // - Add the current identifier which is the first identifier in the chain of accesses.
                // - Reverse the vec since it was created from the back.
                let struct_move_path = struct_move_path.map(|mut path| {
                    path.push(expression_id.to_owned());
                    path.reverse();
                    path
                });

                match &mut variable.move_context {
                    Some(context) => {
                        let MoveContext::Struct(struct_context) = context else {
                            unreachable!("move context can only exist in case of structs since only structs can be partially moved");
                        };
                        let Some(moved_path) = struct_move_path else {
                            unreachable!("struct move path must be set on this branch");
                        };

                        struct_context.mark_moved(
                            moved_path.as_slice(),
                            moved_into,
                            moved_at.into(),
                        );
                    }
                    None => {
                        let context = match struct_move_path {
                            Some(moved_path) => MoveContext::new_struct(
                                moved_into,
                                moved_at.into(),
                                moved_path.as_slice(),
                            ),
                            None => MoveContext::new_basic(moved_into.to_owned(), moved_at.into()),
                        };

                        variable.move_context = Some(context);
                    }
                }
            }
            _ => (),
        }
    }

    fn clear_moved(&mut self, variable: &str) {
        let variable = self
            .find_local_variable_mut(variable)
            .expect("type checking of the expression must have suceeded before");
        variable.move_context = None;
    }

    /// Checks whether `source_type` can be used as `target_type`.
    fn type_usage_as(
        &self,
        source_located_type: &LocatedType,
        target_type: &Type,
    ) -> Result<(), CompileError> {
        let source_type = &*source_located_type.typ.borrow();

        // Check if types match trivially.
        if source_type.kind == target_type.kind {
            return Ok(());
        }

        // Otherwise check if a struct can be passed as a trait.
        // TODO: Adapt for all types.
        if let (
            TypeKind::Struct(_),
            TypeKind::Trait(Trait {
                name: trait_name, ..
            }),
        ) = (&source_type.kind, &target_type.kind)
        {
            if source_type.traits.contains_key(trait_name) {
                return Ok(());
            } else {
                return Err(CompileError::new_migration(
                    source_located_type.token_range.clone(),
                    format!(
                        "Parameter does not implement trait `{}`.\nExpected: {}\nSupplied: {}.",
                        trait_name, target_type, source_type
                    ),
                ));
            }
        }

        Err(CompileError::new_migration(
            source_located_type.token_range.clone(),
            format!(
                "Parameter has incompatible type.\nExpected: {}\nSupplied: {}.",
                target_type, source_type
            ),
        ))
    }

    // TODO: Add self in function type if exists.
    fn generate_function_type(
        &self,
        name: &str,
        params_tokens: &Vec<(IdentifierToken, IdentifierToken)>,
        return_token: &Option<IdentifierToken>,
        _stmt: &Statement,
    ) -> Result<LocatedType, CompileError> {
        let mut params = Vec::new();
        for (_, type_token) in params_tokens {
            let ty = Rc::downgrade(&self.lookup_type_ref(type_token)?);
            params.push(ty);
        }
        let result = if let Some(return_ty) = return_token {
            Some(Rc::downgrade(&self.lookup_type_ref(return_ty)?))
        } else {
            None
        };

        Ok(LocatedType::new_empty_range(wrap_typekind(TypeKind::Func(
            Function {
                name: name.to_owned(),
                params,
                result,
            },
        ))))
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

    fn add_local(&mut self, name: String, dtype: LocatedType) {
        self.locals
            .push(Variable::new(name, self.scope_depth, dtype));
    }

    fn add_local_to_next_scope(&mut self, name: String, dtype: LocatedType) {
        self.scope_depth += 1;
        self.add_local(name, dtype);
        self.scope_depth -= 1;
    }

    fn find_local_variable(&self, id: &str) -> Option<(&Variable, u8)> {
        self.locals
            .iter()
            .rev()
            .enumerate()
            .find(|elem| elem.1.identifier == *id)
            .map(|(pos, element)| {
                let index: u8 = (self.locals.len() - 1 - pos).try_into().unwrap();
                (element, index)
            })
    }

    fn find_local_variable_mut(&mut self, id: &str) -> Option<&mut Variable> {
        self.locals
            .iter_mut()
            .rev()
            .find(|elem| elem.identifier == *id)
    }

    fn print_symbol_table(&self) {
        println!("\nSymbol Table");
        println!("============");
        for (name, symbol) in self.symbol_table.iter() {
            let symbol_ref = &*symbol.borrow();
            match &symbol_ref.kind {
                TypeKind::Func(_) => {
                    println!("{name} ({})", symbol_ref)
                }
                _ => println!("{}", symbol_ref),
            }
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser, util};
    use std::path::Path;

    fn lex_parse_check(path: &str) -> Result<(), CompileError> {
        let test_path: &Path = "src/test_input/type_checker/".as_ref();
        let input = util::file_to_string(&test_path.join(path)).unwrap();
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let mut parser = Parser::new(&lexer);
        let tree = parser.parse().unwrap();
        let mut checker = crate::type_checker::TypeChecker::new();
        let check_result = checker.check(&tree, false);

        if let Err(err) = &check_result {
            util::print_error(&input, err.clone());
        }

        check_result
    }

    #[test]
    fn test_no_return() {
        let res = lex_parse_check("no_return.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_multiple_returns() {
        let res = lex_parse_check("multiple_returns.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_return_type_void() {
        let res = lex_parse_check("return_type_void.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_empty_return() {
        let res = lex_parse_check("empty_return.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_fn_arg() {
        let res = lex_parse_check("fn_arg.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Parameter has incompatible type"));
    }

    #[test]
    fn test_if_condition() {
        let res = lex_parse_check("if_condition.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Condition must be of type bool"));
    }

    #[test]
    fn test_local_binary_plus() {
        let res = lex_parse_check("local_binary_plus.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Condition must be of type bool"));
    }

    #[test]
    fn test_local_binary_greater() {
        let res = lex_parse_check("local_binary_greater.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("not compatible in binary operation"));
    }

    #[test]
    fn test_local_unary_bang() {
        let res = lex_parse_check("local_unary_bang.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("can not be used with a ! operator"));
    }

    #[test]
    fn test_redeclare_struct() {
        let res = lex_parse_check("redeclare_struct.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("already declared in this scope"));
    }

    #[test]
    fn test_struct_init_too_many_fields() {
        let res = lex_parse_check("struct_init_too_many.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Expected 2 fields, but 3 were given"));
    }

    #[test]
    fn test_struct_init_wrong_field_name() {
        let res = lex_parse_check("struct_init_wrong_field_name.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("has no field with name"));
    }

    #[test]
    fn test_struct_init_wrong_type() {
        let res = lex_parse_check("struct_init_wrong_type.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("but type i32 was found"));
    }

    #[test]
    fn test_struct_access_use() {
        let res = lex_parse_check("struct_access_use.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Type i32 can not be used with a ! operator"));
    }

    #[test]
    fn test_struct_access_assign() {
        let res = lex_parse_check("struct_access_assign.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Expression of type str can not be assigned to variable of type i32"));
    }

    #[test]
    fn test_struct_access_impl() {
        let res = lex_parse_check("struct_access_impl.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Type NumWrap not declared in this scope."));
    }

    #[test]
    fn test_trait_impl_method_missing() {
        let res = lex_parse_check("trait_impl_method_missing.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Not all trait items implemented, missing fmt"));
    }

    #[test]
    fn test_trait_impl_type_mismatch() {
        let res = lex_parse_check("trait_impl_type_mismatch.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Type mismatch, expected"));
    }

    #[test]
    fn trait_implementor_struct_passed_as_trait() {
        let res = lex_parse_check("trait_implementor_struct_passed_as_trait.io");
        assert!(res.is_ok());
    }

    #[test]
    fn non_trait_implementor_struct_passed_as_trait() {
        let res = lex_parse_check("non_trait_implementor_struct_passed_as_trait.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not implement trait"));
    }

    #[test]
    fn move_with_variable_declaration() {
        let res = lex_parse_check("move_with_variable_declaration.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "y"
        ));
    }

    #[test]
    fn move_with_assignment() {
        let res = lex_parse_check("move_with_assignment.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "y"
        ));
    }

    #[test]
    fn move_with_struct_init() {
        let res = lex_parse_check("move_with_struct_init.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "num"
        ));
    }

    #[test]
    fn move_with_function_call() {
        let res = lex_parse_check("move_with_function_call.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "addOne"
        ));
    }

    #[test]
    fn move_struct_partially() {
        let res = lex_parse_check("move_struct_partially.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "book" && moved_into == "y"
        ));
    }
}
