use indexmap::IndexMap;

use crate::types::{
    AdhocTypeKind, CompilationErrorKind, CompileError, Declaration, Expression, ExpressionKind,
    IdentifierToken, LocatedType, MethodDeclaration, MethodHeader, MoveContext, Moved, Program,
    ProtoArena, ProtoFunction, ProtoStruct, ProtoTrait, Prototype, PrototypeId, PrototypeKind,
    Statement, Token, TokenKind, TokenRange, Type, TypeName, Variable, SELF,
};
use std::{collections::HashMap, convert::TryInto};

pub struct TypeChecker {
    locals: Vec<Variable>,
    scope_depth: u8,
    symbols_to_check: IndexMap<TypeName, Declaration>,
    prototypes: ProtoArena,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut type_checker = TypeChecker {
            locals: vec![],
            scope_depth: 0,
            symbols_to_check: IndexMap::new(),
            prototypes: ProtoArena::new(),
        };

        type_checker.add_predefined_symbol(TypeName::BOOL, PrototypeKind::Bool);
        type_checker.add_predefined_symbol(TypeName::Integer, PrototypeKind::Integer);
        type_checker.add_predefined_symbol(TypeName::VOID, PrototypeKind::Void);
        type_checker.add_predefined_symbol(TypeName::Double, PrototypeKind::Double);
        type_checker.add_predefined_symbol(TypeName::STR, PrototypeKind::Str);

        type_checker
    }

    fn add_predefined_symbol(&mut self, name: impl Into<TypeName>, typ: PrototypeKind) {
        let name = name.into();
        let identifier = IdentifierToken::new_debug(name.name());
        self.prototypes
            .insert(identifier, Prototype::new(typ))
            .expect("predefined types should not already exist");
    }

    pub fn check(
        &mut self,
        program: Program,
        print_symbol_table: bool,
    ) -> Result<(), CompileError> {
        if program.is_empty() {
            return Ok(());
        }

        // TODO: Check for uniqueness while filling the hashmap.
        self.symbols_to_check = program
            .into_iter()
            .map(|decl| (TypeName::from(decl.identifier()), decl))
            .collect();

        // Not an optimal iteration strategy: We will iterate over symbols that have already been checked,
        // but they will immediately return a "cache hit" from the prototypes.
        for i in 0..self.symbols_to_check.len() {
            self.build_prototype(
                &self
                    .symbols_to_check
                    .get_index(i)
                    .expect("the index is guaranteed to exist")
                    .1
                    .identifier(),
            )?;
        }

        if print_symbol_table {
            self.print_symbol_table();
        }

        let mut result = None;
        loop {
            if let Some(symbol) = self.symbols_to_check.keys().next().map(|sym| sym.clone()) {
                // Hack to be able to call check_symbol. Fix somehow.
                let id_token = self
                    .symbols_to_check
                    .get(&symbol)
                    .expect("it should be present")
                    .identifier()
                    .clone();
                let inner_result = self.check_symbol(None, &id_token);
                if inner_result.is_err() {
                    let _ = result.insert(inner_result);
                    break;
                }
            } else {
                break;
            }
        }

        match result {
            Some(res) => res.map(|_| ()),
            None => Ok(()),
        }
    }

    fn build_prototype(
        &mut self,
        symbol: &IdentifierToken,
    ) -> Result<Option<PrototypeId>, CompileError> {
        let err = match self.prototypes.lookup(symbol) {
            Ok(prototype_id) => {
                return Ok(Some(prototype_id));
            }
            Err(err) => err,
        };

        let type_name: TypeName = symbol.clone().into();
        let declaration = self.symbols_to_check.get(&type_name).cloned().ok_or(err)?;

        self.build_prototype_from_declaration(declaration)
            .and_then(|prototype| {
                prototype
                    .map(|prototype| self.prototypes.insert(symbol.clone(), prototype))
                    .transpose()
            })
    }

    fn build_prototype_from_declaration(
        &mut self,
        decl: Declaration,
    ) -> Result<Option<Prototype>, CompileError> {
        match decl {
            Declaration::StructDecl { identifier, fields } => {
                Ok(Some(Prototype::new(PrototypeKind::Struct(ProtoStruct {
                    name: identifier.clone(),
                    fields: fields.clone(),
                }))))
            }
            Declaration::TraitDecl {
                trait_identifier,
                methods,
            } => {
                let mut headers = HashMap::new();
                let mut bodies = HashMap::new();

                for method in methods.into_iter() {
                    let method_name = method.name.as_str().to_owned();

                    let header = MethodHeader {
                        name: method.name,
                        method_self: method.self_,
                        parameters: method.params,
                        return_type: method.return_ty,
                    };
                    headers.insert(method_name.clone(), header);
                    bodies.insert(method_name.clone(), method.body);
                }

                Ok(Some(
                    Prototype::new(PrototypeKind::Trait(ProtoTrait {
                        name: trait_identifier.clone(),
                        method_bodies: bodies,
                    }))
                    .with_methods(headers),
                ))
            }
            Declaration::FnDecl {
                identifier,
                params,
                return_ty,
                ..
            } => Ok(Some(Prototype::new(PrototypeKind::Function(
                ProtoFunction {
                    name: identifier.clone(),
                    params: params
                        .iter()
                        .map(|(_, param_type)| param_type)
                        .cloned()
                        .collect(),
                    result: return_ty.as_ref().cloned(),
                },
            )))),
            Declaration::ImplMethodDecl {
                type_name,
                method,
                trait_name,
            } => {
                let prototype_id: arenas::EntryId = self
                    .build_prototype(&type_name)?
                    .expect("types should return a prototype id");
                self.prototypes.lookup(&type_name)?;
                let method_header = MethodHeader::from(&method);
                let prototype = self.prototypes.get_mut(prototype_id);
                // TODO: This doesn't differentiate between inherent and trait methods for now.
                prototype
                    .methods
                    .insert(method_header.name.as_str().to_owned(), method_header);
                if let Some(trait_name) = trait_name {
                    // We might insert the trait multiple times, but since it's a set, that's fine.
                    prototype.traits.insert(trait_name);
                }
                Ok(None)
            }
        }
    }

    pub fn check_symbol(
        &mut self,
        dependent: Option<&TypeName>,
        identifier: &IdentifierToken,
    ) -> Result<LocatedType, CompileError> {
        if let Some(dependent) = dependent {
            println!("{dependent} depends on {identifier}");
        } else {
            println!("checking symbol {identifier}");
        }

        if let Some(declaration) = self
            .symbols_to_check
            .shift_remove(&TypeName::from(identifier.clone()))
        {
            return self.check_decl(&declaration);
        }

        self.lookup_located_type(identifier)
    }

    fn check_decl(&mut self, decl: &Declaration) -> Result<LocatedType, CompileError> {
        match decl {
            Declaration::FnDecl {
                identifier,
                params,
                return_ty,
                body,
            } => self.check_fn_symbol(identifier, params, return_ty, body),
            Declaration::StructDecl {
                identifier,
                fields: token_fields,
            } => {
                log::debug!("symbol struct {}", identifier);
                self.check_struct_symbol(identifier, token_fields)
            }
            Declaration::TraitDecl {
                trait_identifier,
                methods,
            } => self.check_trait_symbol(trait_identifier, methods),
            impl_method @ Declaration::ImplMethodDecl {
                type_name,
                trait_name,
                method,
            } => {
                let qualified_type_name = TypeName::from(impl_method.identifier());
                self.check_impl_method_decl(qualified_type_name, type_name, trait_name, method)?;
                Ok(LocatedType::new(
                    method.name.range,
                    Type::new_adhoc(AdhocTypeKind::Method(method.into())),
                ))
            }
        }
    }

    fn check_fn_symbol(
        &mut self,
        identifier: &IdentifierToken,
        params: &Vec<(IdentifierToken, IdentifierToken)>,
        return_ty: &Option<IdentifierToken>,
        body: &Statement,
    ) -> Result<LocatedType, CompileError> {
        log::debug!("symbol fn {identifier}");
        let fn_type_name = TypeName::from(identifier.as_str().to_owned());

        let return_types =
            self.check_function_body(&TypeName::from(identifier.clone()), params, body)?;
        self.check_function_return_types(&fn_type_name, return_types, return_ty)?;

        self.check_function_type(&fn_type_name, params, return_ty, body)?;

        Ok(LocatedType::new(
            identifier.range,
            Type::new_adhoc(AdhocTypeKind::Function(ProtoFunction {
                name: identifier.clone(),
                params: params
                    .iter()
                    .map(|(_, type_token)| type_token)
                    .cloned()
                    .collect(),
                result: return_ty.clone(),
            })),
        ))
    }

    fn check_function_body(
        &mut self,
        function_name: &TypeName,
        params: &Vec<(IdentifierToken, IdentifierToken)>,
        body: &Statement,
    ) -> Result<Vec<LocatedType>, CompileError> {
        for (name_token, param_token) in params {
            let param_type = self.check_symbol(Some(&function_name), param_token)?;
            // Make the parameters available as locals to the function body
            // so that they can be found & type checked
            self.add_local_to_next_scope(name_token.name.clone(), param_type);
        }

        let return_types = self.check_stmt(&function_name, body)?;
        Ok(return_types)
    }

    fn check_struct_symbol(
        &mut self,
        identifier: &IdentifierToken,
        token_fields: &[(IdentifierToken, IdentifierToken)],
    ) -> Result<LocatedType, CompileError> {
        for (_, field_type_name) in token_fields {
            self.check_symbol(Some(&(identifier.clone().into())), field_type_name)?;
        }

        self.lookup_located_type(identifier)
    }

    fn check_trait_symbol(
        &mut self,
        trait_identifier: &IdentifierToken,
        methods: &Vec<MethodDeclaration>,
    ) -> Result<LocatedType, CompileError> {
        log::debug!("symbol trait {}", trait_identifier);

        for method in methods {
            let MethodDeclaration {
                name: method_name,
                self_: _,
                params,
                return_ty,
                body,
            } = method;
            let trait_method_name = TypeName::from(format!(
                "{}::{}",
                trait_identifier.as_str(),
                method_name.as_str()
            ));
            self.check_function_type(&trait_method_name, params, return_ty, body)?;
        }

        self.lookup_located_type(&trait_identifier)
    }

    fn check_impl_method_decl(
        &mut self,
        qualified_type_name: TypeName,
        type_name: &IdentifierToken,
        // TODO: Add checking whether the method matches the trait declaration.
        trait_name: &Option<IdentifierToken>,
        method: &MethodDeclaration,
    ) -> Result<(), CompileError> {
        let MethodDeclaration {
            name,
            self_,
            params,
            return_ty,
            body,
        } = method;

        self.check_function_type(&qualified_type_name, params, return_ty, body)?;

        let receiver_type = self.check_symbol(Some(&qualified_type_name), type_name)?;

        match self_ {
            Some(method_self) => {
                match &method_self.type_token {
                    Some(type_token) => {
                        if type_token != type_name {
                            // TODO: Unclear if we should call check_symbol here or do a lookup.
                            let specified_type = self.lookup_located_type(type_token)?;
                            return Err(CompileError::new_migration(
                                type_token.range.into(),
                                format!(
                                    "'self' in method {} must have type {}, found {}",
                                    name.as_str(),
                                    type_name,
                                    self.display_type(specified_type),
                                ),
                            ));
                        }
                    }
                    None => {}
                }

                self.add_local_to_next_scope(SELF.to_owned(), receiver_type.clone());
            }
            None => (),
        }

        let return_types = self.check_function_body(&qualified_type_name, params, body)?;
        self.check_function_return_types(&qualified_type_name, return_types, return_ty)?;

        Ok(())
    }

    fn check_stmt(
        &mut self,
        dependent: &TypeName,
        stmt: &Statement,
    ) -> Result<Vec<LocatedType>, CompileError> {
        match stmt {
            Statement::ExpressionStmt(expr) => {
                self.check_expr(dependent, expr)?;
                Ok(vec![])
            }
            Statement::LetBinding(id, expr) => {
                let expr_type = self.check_expr(dependent, expr)?;
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

                Ok(vec![])
            }
            Statement::Block(statements) => {
                self.begin_scope();
                let mut return_types = Vec::new();
                for statement in statements.iter() {
                    return_types.extend(self.check_stmt(dependent, statement)?);
                }
                self.end_scope();
                Ok(return_types)
            }
            Statement::Ret(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let expr_type = self.check_expr(dependent, expr)?;
                    Ok(vec![expr_type])
                } else {
                    let void_type = self.lookup_predefined_type_unchecked(TypeName::VOID);
                    Ok(vec![LocatedType::new_empty_range(void_type)])
                }
            }
            Statement::If(condition, if_branch, else_branch_opt) => {
                let cond_type = self.check_expr(dependent, condition)?;

                if cond_type
                    .typ
                    .prototype_id()
                    .map(|id| self.prototypes.get(id))
                    .map(|typ| typ.kind != PrototypeKind::Bool)
                    .unwrap_or(true)
                {
                    return Err(CompileError::new_migration(
                        condition.tokens.clone().into(),
                        format!(
                            "Condition must be of type bool, got {}",
                            self.display_type(cond_type)
                        ),
                    ));
                }
                let mut return_types = self.check_stmt(dependent, if_branch)?;

                if let Some(else_branch) = else_branch_opt {
                    return_types.extend(self.check_stmt(dependent, else_branch)?);
                }

                Ok(return_types)
            }
            Statement::While(condition, body) => {
                let cond_type = self.check_expr(dependent, condition)?;
                if cond_type
                    .typ
                    .prototype_id()
                    .map(|id| self.prototypes.get(id))
                    .map(|typ| typ.kind != PrototypeKind::Bool)
                    .unwrap_or(true)
                {
                    return Err(CompileError::new_migration(
                        condition.tokens.clone().into(),
                        format!(
                            "Condition must be of type bool, got {}",
                            self.display_type(cond_type)
                        ),
                    ));
                }

                self.check_stmt(dependent, body)?;

                Ok(vec![])
            }
            Statement::Print(expr) => {
                self.check_expr(dependent, expr)?;
                Ok(vec![])
            }
        }
    }

    fn check_expr(
        &mut self,
        dependent: &TypeName,
        expr: &Expression,
    ) -> Result<LocatedType, CompileError> {
        match &expr.kind {
            ExpressionKind::Binary(lexpr, op_token, rexpr) => {
                let ltype = self.check_expr(dependent, lexpr)?;
                let rtype = self.check_expr(dependent, rexpr)?;
                let resolved_ltype = ltype.typ.resolve(&self.prototypes);
                let resolved_rtype = rtype.typ.clone().resolve(&self.prototypes);

                if resolved_ltype == resolved_rtype {
                    if [
                        TokenKind::EqualEqual,
                        TokenKind::Greater,
                        TokenKind::Less,
                        TokenKind::LessEqual,
                        TokenKind::GreaterEqual,
                    ]
                    .contains(&op_token.kind)
                    {
                        let bool_type = self.lookup_predefined_type_unchecked(TypeName::BOOL);
                        Ok(LocatedType::new(expr.tokens.clone(), bool_type))
                    } else {
                        Ok(LocatedType::new(expr.tokens.clone(), rtype.typ))
                    }
                } else {
                    Err(CompileError::new_migration(
                        op_token.range.into(),
                        format!(
                            "Types {} and {} are not compatible in binary operation",
                            resolved_ltype, resolved_rtype
                        ),
                    ))
                }
            }
            ExpressionKind::Unary(op, rexpr) => {
                let expr_type = self.check_expr(dependent, rexpr)?;
                match op.kind {
                    TokenKind::Bang => {
                        if expr_type
                            .typ
                            .prototype_id()
                            .map(|id| self.prototypes.get(id))
                            .map(|typ| typ.kind == PrototypeKind::Bool)
                            .unwrap_or(false)
                        {
                            Ok(LocatedType::new(
                                expr_type.token_range.clone(),
                                self.lookup_predefined_type_unchecked(TypeName::BOOL),
                            ))
                        } else {
                            Err(CompileError::new_migration(
                                rexpr.tokens.clone().into(),
                                format!(
                                    "Type {} can not be used with a ! operator",
                                    self.display_type(expr_type)
                                ),
                            ))
                        }
                    }
                    TokenKind::Minus => {
                        if expr_type
                            .typ
                            .prototype_id()
                            .map(|id| self.prototypes.get(id))
                            .map(|typ| typ.kind == PrototypeKind::Integer)
                            .unwrap_or(false)
                        {
                            Ok(LocatedType::new(
                                expr_type.token_range.clone(),
                                self.lookup_predefined_type_unchecked(TypeName::Integer),
                            ))
                        } else {
                            Err(CompileError::new_migration(
                                expr_type.token_range.clone(),
                                format!(
                                    "Type {} can not be used with a - operator",
                                    self.display_type(expr_type)
                                ),
                            ))
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ExpressionKind::Integer { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                self.lookup_predefined_type_unchecked(TypeName::Integer),
            )),
            ExpressionKind::Double { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                self.lookup_predefined_type_unchecked(TypeName::Double),
            )),
            ExpressionKind::Str { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                self.lookup_predefined_type_unchecked(TypeName::STR),
            )),
            ExpressionKind::True { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                self.lookup_predefined_type_unchecked(TypeName::BOOL),
            )),
            ExpressionKind::False { .. } => Ok(LocatedType::new(
                expr.tokens.clone(),
                self.lookup_predefined_type_unchecked(TypeName::BOOL),
            )),
            ExpressionKind::Assign { target, value } => {
                // Lookup type manually to avoid the move check in check_expr. A better solution would be welcome.
                let (assignment_identifier, target_type, is_access): (String, LocatedType, bool) =
                    match &target.kind {
                        ExpressionKind::Identifier(id) => {
                            if let Some((_, index)) = self.find_local_variable(id) {
                                (
                                    id.to_owned(),
                                    self.locals[index as usize].dtype.clone(),
                                    false,
                                )
                            } else {
                                return Err(CompileError::new_migration(
                           expr.tokens.clone().into(),
                           format!(
                              "{} is not defined in the current scope. (Globals are unimplemented.)",
                              id
                          ),
                          ));
                            }
                        }
                        ExpressionKind::Access { name, .. } => {
                            let target_type = self.check_expr(dependent, target)?;
                            (name.unwrap_identifier(), target_type, true)
                        }
                        other => {
                            panic!(
                          "parser should only allow identifier or struct access as assignment target, received {:?}",
                          other
                      )
                        }
                    };

                let value_type = self.check_expr(dependent, value)?;
                let resolved_value_type = value_type.typ.resolve(&self.prototypes);
                let resolved_target_type = target_type.clone().typ.resolve(&self.prototypes);

                if resolved_target_type != resolved_value_type {
                    return Err(CompileError::new_migration(
                        value_type.token_range.clone(),
                        format!(
                            "Expression of type {} can not be assigned to variable of type {}",
                            resolved_value_type, resolved_target_type
                        ),
                    ));
                }

                // Only clear the move if it is not a struct access.
                // This is not because it would be bad, just because it is easier to implement right now.
                // We need to pass the entire assign to the clear_moved so it can check for access, otherwise
                // the assignment identifier is just the field name that is being accessed, but not a variable itself.
                // We could consider the case where `world.players` is moved out of and then assigned again,
                // to be considered a whole struct again after said assignment.
                if !is_access {
                    // Even if the variable that was assigned to was previously moved,
                    // assigning means it contains a new value and is thus considered unmoved.
                    self.clear_moved(&assignment_identifier);
                }
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
                } else if id == SELF {
                    Err(CompileError::new_migration(
                        expr.tokens.clone().into(),
                        "method does not have 'self' as a receiver.".to_string(),
                    ))
                } else {
                    self.check_symbol(
                        Some(dependent),
                        &IdentifierToken::new(expr.tokens.clone(), id.to_owned()),
                    )
                    .map(|typ| typ)
                }
            }
            ExpressionKind::Call(callee, params) => {
                let callee_located_type = self.check_expr(dependent, callee)?;
                let callee_typ = callee_located_type.typ.clone().resolve(&self.prototypes);

                match &callee_typ.kind {
                    PrototypeKind::Method(method) => self.check_function_call(
                        dependent,
                        callee_located_type.token_range,
                        &method.name,
                        // TODO: Avoid cloning.
                        &method
                            .parameters
                            .iter()
                            .map(|(_, param_type)| param_type)
                            .cloned()
                            .collect(),
                        params,
                        &method.return_type,
                    ),
                    PrototypeKind::Function(function) => self.check_function_call(
                        dependent,
                        callee_located_type.token_range,
                        &function.name,
                        &function.params,
                        params,
                        &function.result,
                    ),
                    _ => Err(CompileError::new_migration(
                        callee_located_type.token_range.clone(),
                        format!(
                            "Type {} is not callable.",
                            self.display_type(callee_located_type)
                        ),
                    )),
                }
            }
            ExpressionKind::StructInit { name, values } => {
                let lookup_token =
                    IdentifierToken::new(expr.tokens.clone(), name.unwrap_identifier());
                Token::from_range(&expr.tokens, TokenKind::IdToken(name.unwrap_identifier()));

                let struct_type = self.check_symbol(Some(dependent), &lookup_token)?;
                let resolved_struct_type = struct_type.typ.clone().resolve(&self.prototypes);

                if let PrototypeKind::Struct(declared_struct) = resolved_struct_type.kind {
                    let declared_field_number = declared_struct.fields.len();
                    let given_field_number = values.len();
                    if declared_field_number != given_field_number {
                        return Err(CompileError::new_migration(
                            expr.tokens.clone().into(),
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

                            if declared_name.as_str() != given_name.unwrap_identifier() {
                                return Err(CompileError::new_migration(
                                    given_name.tokens.clone().into(),
                                    format!(
                                        "{} has no field with name {}",
                                        declared_struct.name,
                                        given_name.unwrap_identifier()
                                    ),
                                ));
                            }

                            let given_type = self.check_expr(dependent, given_expr)?;
                            let declared_type =
                                self.check_symbol(Some(dependent), declared_type)?;

                            let resolved_declared_type =
                                declared_type.typ.resolve(&self.prototypes);
                            let resolved_given_type =
                                given_type.typ.clone().resolve(&self.prototypes);

                            if resolved_declared_type != resolved_given_type {
                                return Err(CompileError::new_migration(
                                    given_type.token_range.clone(),
                                    format!(
                                        "Expected {} for field {} but type {} was found",
                                        resolved_declared_type,
                                        declared_name,
                                        self.display_type(given_type)
                                    ),
                                ));
                            }

                            self.move_variable(
                                &given_expr.kind,
                                declared_name.name.clone(),
                                given_expr.tokens.clone(),
                                None,
                            );
                        }
                    }
                } else {
                    return Err(CompileError::new_migration(
                        expr.tokens.clone().into(),
                        "Cannot instantiate anything other than a struct".to_owned(),
                    ));
                }
                Ok(struct_type)
            }
            ExpressionKind::Access { expr, name } => {
                let expr_located_type = self.check_expr(dependent, expr)?;
                let expr_type = expr_located_type
                    .typ
                    .prototype_id()
                    .map(|id| self.prototypes.get(id))
                    .ok_or_else(|| {
                        CompileError::new_migration(
                            expr.tokens.clone().into(),
                            format!("should be a prototype?!"),
                        )
                    })?;
                let access_name = name.unwrap_identifier();

                if let Some(method) = expr_type.methods.get(&access_name) {
                    return Ok(LocatedType::new(
                        expr.tokens.clone(),
                        Type::new_adhoc(AdhocTypeKind::Method(method.clone())),
                    ));
                }

                let mut fields_iter = match &expr_type.kind {
                    PrototypeKind::Struct(strct) => strct.fields.iter(),
                    _ => {
                        // If it's not a struct, the error message should reference the not found method.
                        return Err(CompileError::new_migration(
                            expr.tokens.clone().into(),
                            format!(
                                "Type {} does not have a method named `{}`",
                                expr_type, access_name
                            ),
                        ));
                    }
                };

                let field_type = fields_iter.find(|elem| elem.0.as_str() == access_name);

                let field_type = field_type.ok_or_else(|| {
                    CompileError::new_migration(
                        name.tokens.clone().into(),
                        format!(
                            "Struct {} has no field or method named {}",
                            expr_type, access_name
                        ),
                    )
                })?;

                self.lookup_located_type(&field_type.1.clone())
            }
        }
    }

    fn check_function_return_types(
        &mut self,
        dependent: &TypeName,
        return_types: Vec<LocatedType>,
        return_token: &Option<IdentifierToken>,
    ) -> Result<(), CompileError> {
        let declared_ret_type = if let Some(return_type) = return_token {
            self.check_symbol(Some(dependent), return_type)?
        } else {
            LocatedType::new_empty_range(self.lookup_predefined_type_unchecked(TypeName::VOID))
        };
        let resolved_declared_ret_type = declared_ret_type.typ.resolve(&self.prototypes);

        if resolved_declared_ret_type.kind != PrototypeKind::Void && return_types.is_empty() {
            return Err(CompileError::new_migration(
                declared_ret_type.token_range.clone(),
                format!(
                    "This function has to return a type {}.",
                    resolved_declared_ret_type
                ),
            ));
        }

        for return_type in return_types {
            let resolved_return_type = return_type.typ.resolve(&self.prototypes);
            if resolved_return_type != resolved_declared_ret_type {
                return Err(CompileError::new_migration(
                    return_type.token_range.clone(),
                    format!(
                        "Returned type {} does not match declared return type {}.",
                        resolved_return_type, resolved_declared_ret_type
                    ),
                ));
            }
        }
        Ok(())
    }

    fn check_function_call(
        &mut self,
        dependent: &TypeName,
        callee_location: TokenRange,
        name: &IdentifierToken,
        expected_params: &Vec<IdentifierToken>,
        given_params: &Vec<Expression>,
        result: &Option<IdentifierToken>,
    ) -> Result<LocatedType, CompileError> {
        if expected_params.len() != given_params.len() {
            return Err(CompileError::new_migration(
                callee_location,
                format!(
                    "{} needs {} parameters, but {} were supplied.",
                    name,
                    expected_params.len(),
                    given_params.len()
                ),
            ));
        }

        for (expected_param, given_param) in expected_params.iter().zip(given_params.iter()) {
            let call_param_type = self.check_expr(dependent, given_param)?;

            let expected_param_type = self.check_symbol(Some(dependent), expected_param)?;
            self.type_usage_as(&call_param_type, &expected_param_type)?;
            self.move_variable(
                &given_param.kind,
                name.name.clone(),
                given_param.tokens.clone(),
                None,
            );
        }

        Ok(if let Some(res) = &result {
            self.check_symbol(Some(dependent), res)?
        } else {
            LocatedType::new_empty_range(self.lookup_predefined_type_unchecked(TypeName::VOID))
        })
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

    fn check_function_type(
        &mut self,
        dependent: &TypeName,
        params_tokens: &Vec<(IdentifierToken, IdentifierToken)>,
        return_token: &Option<IdentifierToken>,
        _stmt: &Statement,
    ) -> Result<(), CompileError> {
        for (_, type_token) in params_tokens {
            self.check_symbol(Some(dependent), type_token)?;
        }

        if let Some(return_ty) = return_token {
            self.check_symbol(Some(dependent), return_ty)?;
        }

        Ok(())
    }

    /// Checks whether `source_type` can be used as `target_type`.
    fn type_usage_as(
        &self,
        source_located_type: &LocatedType,
        target_type: &LocatedType,
    ) -> Result<(), CompileError> {
        let source_type = &source_located_type.typ;
        let resolved_source_type = source_type.clone().resolve(&self.prototypes);
        let resolved_target_type = target_type.typ.clone().resolve(&self.prototypes);

        // Check if types match trivially.
        if resolved_source_type == resolved_target_type {
            return Ok(());
        }

        // Otherwise check if a struct can be passed as a trait.
        // TODO: Adapt for all types.
        if let (
            PrototypeKind::Struct(_),
            PrototypeKind::Trait(ProtoTrait {
                name: trait_name, ..
            }),
        ) = (&resolved_source_type.kind, &resolved_target_type.kind)
        {
            if resolved_source_type.traits.contains(trait_name) {
                return Ok(());
            } else {
                return Err(CompileError::new_migration(
                    source_located_type.token_range.clone(),
                    format!(
                        "Parameter does not implement trait `{}`.\nExpected: {}\nSupplied: {}.",
                        trait_name, resolved_target_type, resolved_source_type
                    ),
                ));
            }
        }

        Err(CompileError::new_migration(
            source_located_type.token_range.clone(),
            format!(
                "Parameter has incompatible type.\nExpected: {}\nSupplied: {}.",
                resolved_target_type, resolved_source_type
            ),
        ))
    }

    fn display_type(&self, typ: impl AsRef<Type>) -> String {
        let typ = typ.as_ref();
        match typ {
            Type::Prototype(id) => {
                format!("{}", self.prototypes.get(*id))
            }
            Type::Adhoc(ad_hoc) => {
                format!("{}", ad_hoc)
            }
        }
    }

    fn lookup_located_type(
        &mut self,
        identifier: &IdentifierToken,
    ) -> Result<LocatedType, CompileError> {
        self.prototypes
            .lookup(identifier)
            .map(|id| LocatedType::new(identifier.range, Type::new_prototype(id)))
    }

    fn lookup_predefined_type_unchecked(&self, type_name: TypeName) -> Type {
        let identifier = IdentifierToken::new_debug(type_name.name());
        self.prototypes
            .lookup(&identifier)
            .map(|id| Type::new_prototype(id))
            .expect("predefined types should exist")
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
        for (_, symbol) in self.prototypes.types.iter_items() {
            println!("{symbol}");
        }
        println!();
    }
}
