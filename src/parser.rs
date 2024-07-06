use crate::lexer::Lexer;
use crate::types::TokenKind::{self, *};
use crate::types::{
    CompileError,
    Declaration::{self, *},
    Expression,
    ExpressionKind::*,
    Program,
    Statement::{self, *},
};
use crate::types::{IdentifierToken, MethodDeclaration, MethodHeader, MethodSelf, Token, SELF};
use std::convert::TryInto;
use std::ops::Range;

type StatementResult = Result<Statement, CompileError>;
type ExpressionResult = Result<Expression, CompileError>;
type DeclarationResult = Result<Declaration, CompileError>;

pub struct Parser<'a> {
    lexer: &'a Lexer,
    current: usize,
    whitespace_stack: Vec<u8>,
}

#[derive(Debug, PartialEq)]
enum CallableKind {
    Function,
    Method,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Program, CompileError> {
        self.program()
    }

    fn program(&mut self) -> Result<Program, CompileError> {
        let mut prog = Vec::new();
        while !self.is_at_end() {
            prog.push(self.declaration()?);
        }
        Ok(prog)
    }

    // Declarations

    fn declaration(&mut self) -> DeclarationResult {
        match &self.peek().kind {
            VarToken => self.variable_declaration(),
            IdToken(_) => {
                if self.is_callable_declaration() {
                    self.function_declaration()
                } else {
                    Ok(StatementDecl(self.statement()?))
                }
            }
            TraitToken => self.trait_declaration(),
            StructToken => self.struct_declaration(),
            ImplToken => self.impl_declaration(),
            WhiteSpace(_) => Err(self.error(self.peek().clone().into(), "Unexpected whitespace")),
            NewLine => {
                self.advance();
                self.declaration()
            }
            _ => Ok(StatementDecl(self.statement()?)),
        }
    }

    fn trait_declaration(&mut self) -> DeclarationResult {
        self.advance();

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected an identifier as trait name.",
            ));
        }
        let name = self.advance_identifier()?;

        let mut methods = Vec::new();

        self.expect_newline()?;

        if self.set_next_indentation()? {
            while self.has_same_indentation()? {
                if self.is_trait_method_declaration() {
                    methods.push(self.trait_method_declaration()?);
                } else {
                    return Err(
                        self.error(self.peek().clone().into(), "Expected method declaration")
                    );
                }
            }

            self.pop_indentation();
        }

        Ok(TraitDecl {
            trait_identifier: name,
            methods,
        })
    }

    fn struct_declaration(&mut self) -> DeclarationResult {
        self.advance();

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected an identifier as struct name.",
            ));
        }
        let struct_identifier = self.advance_identifier()?;

        let mut fields = Vec::new();

        self.expect_newline()?;

        // This could easily be removed and handled in set_next_indentation if
        // structs without fields wouldn't be allowed
        if self.id_and_colon_tokens_follow() {
            return Err(self.error(self.peek().clone().into(), "Expected fields to be indented"));
        }

        if self.set_next_indentation()? {
            while self.has_same_indentation()? {
                if !self.peek().is_id_token() {
                    return Err(self.error(
                        self.peek().clone().into(),
                        "Expected an identifier as field name.",
                    ));
                }

                let field_identifier = self.advance_identifier()?;

                self.consume(Colon, "Expected ':' after name.")?;

                if !self.peek().is_id_token() {
                    return Err(self.error(
                        self.peek().clone().into(),
                        "Expected a type identifier after field name.",
                    ));
                }

                let type_token = self.advance_identifier()?;

                fields.push((field_identifier, type_token));

                self.expect_newline()?;
            }

            self.pop_indentation();
        }

        Ok(StructDecl {
            identifier: struct_identifier,
            fields,
        })
    }

    fn impl_declaration(&mut self) -> DeclarationResult {
        self.advance();

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected an identifier as struct or trait name.",
            ));
        }
        let mut name = self.advance_identifier()?;
        let mut trait_name: Option<IdentifierToken> = None;

        if self.match_(For) {
            if !self.peek().is_id_token() {
                return Err(self.error(
                    self.peek().clone().into(),
                    "Expected an identifier as struct name.",
                ));
            }
            trait_name = Some(name);
            name = self.advance_identifier()?;
        }

        self.expect_newline()?;

        let mut methods = Vec::new();

        if self.set_next_indentation()? {
            while self.has_same_indentation()? {
                if self.is_callable_declaration() {
                    methods.push(self.method_declaration()?);
                } else {
                    return Err(
                        self.error(self.peek().clone().into(), "Expected method declaration")
                    );
                }
            }

            self.pop_indentation();
        }

        Ok(ImplDecl {
            struct_name: name,
            trait_name,
            methods,
        })
    }

    fn variable_declaration(&mut self) -> DeclarationResult {
        // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
        self.advance();
        let id = if let IdToken(id_) = &self.advance().kind {
            Ok(id_.clone())
        } else {
            Err(self.error(
                self.peek().clone().into(),
                "Expected an identifier after variable declaration.",
            ))
        }?;

        self.consume(Equal, "Expected '=' after variable declaration.")?;
        let expr = self.expression()?;
        self.expect_newline()?;
        Ok(VarDecl(id.clone(), expr))
    }

    // Callables

    fn is_trait_method_declaration(&mut self) -> bool {
        if let Some(next) = self.peek_next() {
            matches!(self.peek().kind, IdToken(_)) && matches!(next.kind, LeftParen)
        } else {
            false
        }
    }

    // Checks if the following tokens are a callable declaration.
    // Needs to deep-inspect the tokens in order to differentiate a method call in a statement
    // from a proper declaration, which it does by detecting a new line and whitespace.
    fn is_callable_declaration(&mut self) -> bool {
        let current = self.current;

        let mut is_fn_decl = false;

        while self.peek().kind != EndOfFile {
            match self.advance().kind {
                NewLine => {
                    if let WhiteSpace(indent) = self.peek().kind {
                        if self.exceeds_current_indentation(indent) {
                            is_fn_decl = true;
                        }
                    }
                    break;
                }

                TraitToken => break,
                StructToken => break,
                ImplToken => break,

                _ => (),
            }
        }

        self.current = current;

        is_fn_decl
    }

    fn parse_callable_parameter_type(&mut self) -> Result<IdentifierToken, CompileError> {
        self.consume(Colon, "Expected ':' for type annotation.")?;

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected type identifier after parameter.",
            ));
        }

        self.advance_identifier()
    }

    fn parse_method_self(&mut self) -> Result<Option<MethodSelf>, CompileError> {
        if self.peek().kind != SelfToken {
            return Ok(None);
        }

        self.advance();

        let mut self_type_token: Option<IdentifierToken> = None;

        if self.peek().kind == Colon {
            self_type_token = Some(self.parse_callable_parameter_type()?);
        }

        if !self.match_(Comma) && self.peek().kind != RightParen {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected ')' or ',' after parameter.",
            ));
        }

        Ok(Some(MethodSelf {
            type_token: self_type_token,
        }))
    }

    fn parse_callable_parameters(
        &mut self,
        callable_kind: CallableKind,
    ) -> Result<Vec<(IdentifierToken, IdentifierToken)>, CompileError> {
        let mut params = Vec::new();

        if self.peek().kind != RightParen {
            while self.peek().kind != RightParen {
                let name_token = self.advance().clone();

                if name_token.kind == SelfToken {
                    match callable_kind {
                        CallableKind::Method => {
                            return Err(self.error(
                                self.peek().clone().into(),
                                "'self' can only appear as the first parameter of a method",
                            ));
                        }
                        CallableKind::Function => {
                            return Err(self.error(
                                self.peek().clone().into(),
                                "'self' can only appear in methods",
                            ));
                        }
                    }
                } else {
                    let type_token = self.parse_callable_parameter_type()?;

                    let name_token = name_token.try_into()?;
                    params.push((name_token, type_token));
                }

                if !self.match_(Comma) && self.peek().kind != RightParen {
                    return Err(self.error(
                        self.peek().clone().into(),
                        "Expected ')' or ',' after parameter.",
                    ));
                }
            }

            if params.is_empty() {
                return Err(self.error(self.peek().clone().into(), "Expected identifier."));
            }
        }

        Ok(params)
    }

    fn parse_callable_return_token(&mut self) -> Result<Option<IdentifierToken>, CompileError> {
        let return_token = if self.match_(Arrow) {
            Some(self.advance_identifier()?)
        } else {
            None
        };

        Ok(return_token)
    }

    fn parse_callable_body(&mut self) -> Result<Statement, CompileError> {
        let mut body = self.block()?;

        // Add a return statement if it does not exist
        if let Block(decls) = &mut body {
            if let Some(StatementDecl(Ret(_))) = &decls.last() {
            } else {
                decls.push(StatementDecl(Ret(None)));
            }
        }

        Ok(body)
    }

    fn parse_method_header(&mut self) -> Result<MethodHeader, CompileError> {
        let id = self.advance_identifier()?;
        self.consume(LeftParen, "Expected '(' after method name.")?;

        let method_self = self.parse_method_self()?;
        let params = self.parse_callable_parameters(CallableKind::Method)?;

        self.consume(RightParen, "Expected ')' after method parameters.")?;

        let return_ty = self.parse_callable_return_token()?;

        let header = MethodHeader {
            name: id,
            method_self,
            parameters: params,
            return_type: return_ty,
        };

        Ok(header)
    }

    fn method_declaration(&mut self) -> Result<MethodDeclaration, CompileError> {
        let MethodHeader {
            name,
            method_self,
            parameters,
            return_type,
        } = self.parse_method_header()?;

        let body = self.parse_callable_body()?;

        Ok(MethodDeclaration {
            name,
            self_: method_self,
            params: parameters,
            return_ty: return_type,
            body,
        })
    }

    fn function_declaration(&mut self) -> DeclarationResult {
        let id = self.advance_identifier()?;

        self.consume(LeftParen, "Expected '(' after function name.")?;

        let params = self.parse_callable_parameters(CallableKind::Function)?;

        self.consume(RightParen, "Expected ')' after function parameters.")?;

        let return_ty = self.parse_callable_return_token()?;
        let body = self.parse_callable_body()?;

        Ok(FnDecl {
            identifier: id,
            params,
            return_ty,
            body,
        })
    }

    // A trait method can either be just a method header or also contain a default implementation.
    fn trait_method_declaration(&mut self) -> Result<MethodDeclaration, CompileError> {
        let MethodHeader {
            name,
            method_self,
            parameters,
            return_type,
        } = self.parse_method_header()?;

        // An empty body signals no default implementation.
        let mut body = Statement::Block(vec![]);

        // If the next indent is greater, we expect a method body, otherwise a newline.
        if let NewLine = self.peek().kind {
            match self
                .peek_next()
                .expect("should be EndOfFile in worst case")
                .kind
            {
                WhiteSpace(indent) => {
                    if self.exceeds_current_indentation(indent) {
                        body = self.parse_callable_body()?;
                    } else {
                        self.expect_newline()?;
                    }
                }
                _ => {
                    self.expect_newline()?;
                }
            }
        }

        Ok(MethodDeclaration {
            name,
            self_: method_self,
            params: parameters,
            return_ty: return_type,
            body,
        })
    }

    // Statements

    fn statement(&mut self) -> StatementResult {
        match self.peek().kind {
            PrintToken => self.print_statement(),
            IfToken => self.if_statement(),
            WhileToken => self.while_statement(),
            LeftBrace => self.block(),
            Return => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn block(&mut self) -> StatementResult {
        self.expect_newline()?;
        let mut decls = Vec::new();

        if self.set_next_indentation()? {
            while self.has_same_indentation()? {
                decls.push(self.declaration()?);
            }

            self.pop_indentation();
        }

        Ok(Block(decls))
    }

    fn expression_statement(&mut self) -> StatementResult {
        let res = self.expression()?;
        self.expect_newline()?;
        Ok(ExpressionStmt(res))
    }

    fn print_statement(&mut self) -> StatementResult {
        self.advance();
        let expr = self.expression()?;
        self.expect_newline()?;
        Ok(Print(expr))
    }

    fn return_statement(&mut self) -> StatementResult {
        self.advance();
        if self.match_(NewLine) {
            Ok(Ret(None))
        } else {
            let expr = self.expression()?;
            self.expect_newline()?;
            Ok(Ret(Some(expr)))
        }
    }

    fn if_statement(&mut self) -> StatementResult {
        self.advance();

        let condition_expr = self.expression()?;

        let if_stmt = self.block()?;

        let mut else_stmt = None;

        if self.match_(ElseToken) {
            // Without leading whitespace
            else_stmt = Some(Box::new(self.block()?));
        } else {
            // With leading whitespace
            if let Some(token) = self.peek_next() {
                if let ElseToken = token.kind {
                    if self.has_same_indentation()? {
                        // Consume else
                        self.advance();
                        else_stmt = Some(Box::new(self.block()?));
                    }
                }
            }
        }

        Ok(If(condition_expr, Box::new(if_stmt), else_stmt))
    }

    fn while_statement(&mut self) -> StatementResult {
        self.advance();

        let condition_expr = self.expression()?;

        let body = self.block()?;

        Ok(While(condition_expr, Box::new(body)))
    }

    // Expressions

    fn expression(&mut self) -> ExpressionResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExpressionResult {
        let left_hand = self.equality();

        if self.match_(Equal) {
            let left_hand = left_hand?;
            let equals = self.previous().clone();
            let value = self.assignment()?;
            if let Identifier(_) | Access { .. } = &left_hand.kind {
                return Ok(Expression::new(
                    equals.into(),
                    Assign {
                        target: Box::new(left_hand),
                        value: Box::new(value),
                    },
                ));
            } else {
                return Err(self.error(equals.into(), "Invalid assignment target."));
            }
        }
        left_hand
    }

    fn equality(&mut self) -> ExpressionResult {
        // equality → comparison ( ( "!=" | "==" ) comparison )* ;
        let mut expr = self.comparison()?;
        while self.match_(BangEqual) || self.match_(EqualEqual) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expression::new(
                expr.tokens.start..right.tokens.end,
                Binary(Box::new(expr), operator, Box::new(right)),
            );
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ExpressionResult {
        // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
        let mut expr = self.addition()?;
        while self.match_(Greater)
            || self.match_(GreaterEqual)
            || self.match_(Less)
            || self.match_(LessEqual)
        {
            let operator = self.previous().clone();
            let right = self.addition()?;
            expr = Expression::new(
                expr.tokens.start..right.tokens.end,
                Binary(Box::new(expr), operator, Box::new(right)),
            );
        }
        Ok(expr)
    }

    fn addition(&mut self) -> ExpressionResult {
        // addition → multiplication ( ( "-" | "+" ) multiplication )* ;
        let mut expr = self.multiplication()?;
        while self.match_(Plus) || self.match_(Minus) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expression::new(
                expr.tokens.start..right.tokens.end,
                Binary(Box::new(expr), operator, Box::new(right)),
            );
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> ExpressionResult {
        // multiplication → unary ( ( "/" | "*" ) unary )* ;
        let mut expr = self.unary()?;
        while self.match_(Slash) || self.match_(Star) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expression::new(
                expr.tokens.start..right.tokens.end,
                Binary(Box::new(expr), operator, Box::new(right)),
            );
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExpressionResult {
        // unary → ( "!" | "-" ) unary
        //         | primary ;
        if self.match_(Bang) || self.match_(Minus) {
            let operator = self.previous().clone();
            let lexpr = self.unary()?;
            Ok(Expression::new(
                (operator.range.offset as usize)..lexpr.tokens.end,
                Unary(operator, Box::new(lexpr)),
            ))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ExpressionResult {
        let mut expr = self.struct_instantiation()?;

        loop {
            if self.match_(LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.match_(Dot) {
                let name = self.primary()?;
                if let Identifier(_) = &name.kind {
                    expr = Expression::new(
                        expr.tokens.start..name.tokens.end,
                        Access {
                            expr: Box::new(expr),
                            name: Box::new(name),
                        },
                    );
                } else {
                    return Err(CompileError {
                        token_range: name.tokens.clone(),
                        message: "Expected identifier for field access".into(),
                    });
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expression) -> ExpressionResult {
        let mut params = Vec::new();
        while self.peek().kind != RightParen {
            params.push(self.expression()?);

            if self.peek().kind != Comma {
                break;
            } else {
                self.advance();
            }
        }

        self.consume(RightParen, "Expected ')' to end function call.")?;

        Ok(Expression::new(
            expr.tokens.start..self.current,
            Call(Box::new(expr), params),
        ))
    }

    fn struct_instantiation(&mut self) -> ExpressionResult {
        let mut expr = self.primary()?;

        if let Identifier(_) = &expr.kind {
            if self.match_(LeftBrace) {
                let mut fields = Vec::new();
                while !self.match_(RightBrace) {
                    let field_name = self.primary()?;
                    if let Identifier(_) = &field_name.kind {
                        self.consume(Colon, "Expected ':' in field initializer.")?;
                        let field_value = self.expression()?;
                        self.consume(Comma, "Expected ',' after field initializer.")?;
                        fields.push((field_name, field_value))
                    } else {
                        return Err(CompileError {
                            token_range: field_name.tokens.clone(),
                            message: "Expected identifier as field name".into(),
                        });
                    }
                }
                // TODO: Fix case when struct has size 0.
                let last_token_end = self.current_range().end;
                expr = Expression::new(
                    expr.tokens.start..last_token_end,
                    StructInit {
                        name: Box::new(expr),
                        values: fields,
                    },
                );
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> ExpressionResult {
        // primary → NUMBER | STRING | "false" | "true" | "nil"
        //           | "(" expression ")" ;
        match &self.peek().kind {
            TrueToken => {
                let tok = Expression::new(self.current_range(), True);
                self.advance();
                Ok(tok)
            }
            FalseToken => {
                let tok = Expression::new(self.current_range(), False);
                self.advance();
                Ok(tok)
            }
            Num(int) => {
                let num = Expression::new(self.current_range(), Integer { int: *int });
                self.advance();
                Ok(num)
            }
            FloatNum(float) => {
                let num = Expression::new(self.current_range(), Double { float: *float });
                self.advance();
                Ok(num)
            }
            String_(str_) => {
                let string = Expression::new(
                    self.current_range(),
                    Str {
                        string: str_.clone(),
                    },
                );
                self.advance();
                Ok(string)
            }
            IdToken(str_) => {
                let id = Expression::new(self.current_range(), Identifier(str_.clone()));
                self.advance();
                Ok(id)
            }
            SelfToken => {
                let id = Expression::new(self.current_range(), Identifier(SELF.to_owned()));
                self.advance();
                Ok(id)
            }
            _ => {
                if self.match_(LeftParen) {
                    let expr = self.expression()?;

                    if !self.match_(RightParen) {
                        Err(self.error(self.previous().clone().into(), "Expected ')'."))
                    } else {
                        Ok(expr)
                    }
                } else {
                    Err(self.error(self.peek().clone().into(), "Expected expression."))
                }
            }
        }
    }

    // Helpers

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn match_(&mut self, token: TokenKind) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token: TokenKind) -> bool {
        self.peek().kind == token
    }

    fn peek(&self) -> &Token {
        &self.lexer.tokens[self.current]
    }

    fn peek_next(&self) -> Option<&Token> {
        if self.current + 1 < self.lexer.tokens.len() {
            Some(&self.lexer.tokens[self.current + 1])
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == EndOfFile
    }

    fn previous(&self) -> &Token {
        &self.lexer.tokens[self.current - 1]
    }

    #[allow(clippy::range_plus_one)]
    fn current_range(&self) -> Range<usize> {
        self.lexer.tokens[self.current].clone().into()
    }

    fn set_next_indentation(&mut self) -> Result<bool, CompileError> {
        if let WhiteSpace(indent) = self.peek().kind {
            let last_indent = *self.whitespace_stack.last().unwrap();
            if indent > last_indent {
                self.whitespace_stack.push(indent);
                Ok(true)
            } else {
                Err(self.error(
                    self.peek().clone().into(),
                    &format!("Expected indentation greater than {last_indent} (found {indent})"),
                ))
            }
        } else {
            Ok(false)
        }
    }

    fn has_same_indentation(&mut self) -> Result<bool, CompileError> {
        if let WhiteSpace(indent) = self.peek().kind {
            let mut rev_iter = self.whitespace_stack.iter().rev();
            let current_indent = *rev_iter.next().unwrap();
            let previous_indent = *rev_iter.next().unwrap();
            if previous_indent == indent {
                Ok(false)
            } else if current_indent == indent {
                self.advance();
                Ok(true)
            } else {
                let err_msg = format!(
                    "Expected indentation of {} but got {}",
                    self.whitespace_stack.last().unwrap(),
                    indent
                );
                Err(self.error(self.peek().clone().into(), &err_msg))
            }
        } else {
            Ok(false)
        }
    }

    /// Returns true if the given `indent` exceeds the current indentation.
    fn exceeds_current_indentation(&self, indent: u8) -> bool {
        *self.whitespace_stack.last().unwrap() < indent
    }

    fn expect_newline(&mut self) -> Result<(), CompileError> {
        match self.peek().kind {
            EndOfFile => Ok(()),
            _ => self.consume(NewLine, "Expected newline"),
        }
    }

    fn pop_indentation(&mut self) {
        if self.whitespace_stack.len() > 1 {
            self.whitespace_stack.pop();
        }
    }

    fn id_and_colon_tokens_follow(&mut self) -> bool {
        if let Some(token) = self.peek_next() {
            if let Colon = token.kind {
                self.peek().is_id_token()
            } else {
                false
            }
        } else {
            false
        }
    }

    // Errors

    fn consume(&mut self, token: TokenKind, error_msg: &'static str) -> Result<(), CompileError> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(self.peek().clone().into(), error_msg))
        }
    }

    fn advance_identifier(&mut self) -> Result<IdentifierToken, CompileError> {
        let token = self.advance();
        if let TokenKind::IdToken(id) = &token.kind {
            Ok(IdentifierToken {
                name: id.clone(),
                range: token.range,
            })
        } else {
            Err(CompileError {
                token_range: token.range.into(),
                message: "Expected token to be an identifier".to_string(),
            })
        }
    }

    fn error(&self, token_range: Range<usize>, message: &str) -> CompileError {
        CompileError {
            token_range,
            message: message.into(),
        }
    }

    // fn synchronize(&mut self) {
    //     self.advance();

    //     while !self.is_at_end() {
    //         if self.previous().kind == Semicolon { return; }

    //         match self.peek().kind {
    //             // Class | Fun | Var | For | If | While | Print | Return => {
    //             //     return;
    //             // },
    //             _ => (),
    //         }

    //         self.advance();
    //     }
    // }

    // Misc

    pub fn new(lexer: &'a Lexer) -> Self {
        Parser {
            lexer,
            current: 0,
            whitespace_stack: vec![0],
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::util;

    use super::*;

    macro_rules! dexpr {
    [$exp:expr] => {
            {
                Expression::new_debug($exp)
            }
        };
    }

    macro_rules! expr {
    [$exp:expr] => {
            {
                Box::new(Expression::new_debug($exp))
            }
        };
    }

    macro_rules! token {
        [$token:expr] => {
            {
                Token::new_debug($token)
            }
        };
    }

    fn lex_and_parse(input: &str) -> Program {
        match lex_and_parse_impl(input) {
            Ok(res) => res,
            Err(err) => {
                util::print_error(input, err.token_range, &err.message);
                panic!("lex_and_parse failed");
            }
        }
    }

    fn lex_and_parse_err(input: &str) -> CompileError {
        lex_and_parse_impl(input).unwrap_err()
    }

    fn lex_and_parse_impl(input: &str) -> Result<Program, CompileError> {
        let mut lexer = Lexer::new();
        lexer.lex(input);
        let mut parser = Parser::new(&lexer);
        parser.parse()
    }

    #[test]
    fn test_multiplication() {
        // Test 9 + 1 / 4
        let lexer = Lexer::new_from_tokenkind(vec![Num(9), Plus, Num(1), Slash, Num(4), EndOfFile]);
        let mut parser = Parser::new(&lexer);
        if let StatementDecl(ExpressionStmt(expr)) = &parser.parse().unwrap()[0] {
            let expected = expr![Binary(
                expr![Integer { int: 9 }],
                token![Plus],
                expr![Binary(
                    expr![Integer { int: 1 }],
                    token![Slash],
                    expr![Integer { int: 4 }]
                )],
            )];
            assert_eq!(*expr, *expected);
        } else {
            panic!();
        }
    }

    #[test]
    fn test_unary() {
        // Test "!!false"
        let lexer = Lexer::new_from_tokenkind(vec![Bang, Bang, FalseToken, EndOfFile]);
        let mut parser = Parser::new(&lexer);
        if let StatementDecl(ExpressionStmt(expr)) = &parser.parse().unwrap()[0] {
            assert_eq!(
                *expr,
                *expr![Unary(
                    token![Bang],
                    expr![Unary(token![Bang], expr![False])]
                )]
            );
        } else {
            panic!();
        }
    }

    #[test]
    fn test_struct_decl() {
        let input = "
struct MyStruct
    field1: str
    field2: bool
        ";

        let parse_result = lex_and_parse(input);

        let expected = StructDecl {
            identifier: IdentifierToken::new_debug("MyStruct"),
            fields: vec![
                (
                    IdentifierToken::new_debug("field1"),
                    IdentifierToken::new_debug("str"),
                ),
                (
                    IdentifierToken::new_debug("field2"),
                    IdentifierToken::new_debug("bool"),
                ),
            ],
        };

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_decl_whitespace() {
        let input = "
struct MyStruct
    // A comment
    field1: str

    // Another one!
    field2: bool

        ";

        let parse_result = lex_and_parse(input);

        let expected = StructDecl {
            identifier: IdentifierToken::new_debug("MyStruct"),
            fields: vec![
                (
                    IdentifierToken::new_debug("field1"),
                    IdentifierToken::new_debug("str"),
                ),
                (
                    IdentifierToken::new_debug("field2"),
                    IdentifierToken::new_debug("bool"),
                ),
            ],
        };

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_decl_no_fields() {
        let input = "
// Various amount of whitespace
struct MyStruct
            
   
        ";

        let parse_result = lex_and_parse(input);

        let expected = StructDecl {
            identifier: IdentifierToken::new_debug("MyStruct"),
            fields: vec![],
        };

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_decl_missing_indentation_error() {
        let input = "struct MyStruct\nfield: type";

        let parse_err = lex_and_parse_err(input);

        assert_eq!(parse_err.message, "Expected fields to be indented");
    }

    #[test]
    fn test_fn_decl_success() {
        let input = "
foo(arg: i32) -> i32
    bar()
    x = 10 / 2
    print 3
    return 5";

        let parse_result = lex_and_parse(input);

        let bar_call = Call(expr!(Identifier("bar".into())), vec![]);

        let expected = FnDecl {
            identifier: IdentifierToken::new_debug("foo"),
            params: vec![(
                IdentifierToken::new_debug("arg"),
                IdentifierToken::new_debug("i32"),
            )],
            return_ty: Some(IdentifierToken::new_debug("i32")),
            body: Block(vec![
                StatementDecl(ExpressionStmt(dexpr!(bar_call))),
                StatementDecl(ExpressionStmt(Expression::new_debug(Assign {
                    target: expr!(Identifier("x".into())),
                    value: expr!(Binary(
                        expr!(Integer { int: 10 }),
                        token!(Slash),
                        expr!(Integer { int: 2 }),
                    )),
                }))),
                StatementDecl(Print(Expression::new_debug(Integer { int: 3 }))),
                StatementDecl(Ret(Some(dexpr!(Integer { int: 5 })))),
            ]),
        };
        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_fn_decl_multiline_params() {
        let input = "
long_function_name(
    arg: i32,
    oth: str
) -> i32
    return 5";

        let parse_result = lex_and_parse(input);

        let expected = FnDecl {
            identifier: IdentifierToken::new_debug("long_function_name"),
            params: vec![
                (
                    IdentifierToken::new_debug("arg"),
                    IdentifierToken::new_debug("i32"),
                ),
                (
                    IdentifierToken::new_debug("oth"),
                    IdentifierToken::new_debug("str"),
                ),
            ],
            return_ty: Some(IdentifierToken::new_debug("i32")),
            body: Block(vec![StatementDecl(Ret(Some(dexpr!(Integer { int: 5 }))))]),
        };
        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_fn_decl_indentation_error() {
        let input = "
foo(arg: i32) -> i32
    bar()
    x = 10 / 2
  print 3
    return 5";

        let parse_err = lex_and_parse_err(input);

        assert_eq!(parse_err.message, "Expected indentation of 4 but got 2");
    }

    #[test]
    fn test_while_stmt() {
        let input = "
while i < 3
    i = i + 1
";
        let parse_result = lex_and_parse(input);

        let expected = StatementDecl(While(
            Expression::new_debug(Binary(
                expr!(Identifier("i".into())),
                token!(Less),
                expr!(Integer { int: 3 }),
            )),
            Box::new(Block(vec![StatementDecl(ExpressionStmt(
                Expression::new_debug(Assign {
                    target: expr!(Identifier("i".into())),
                    value: expr!(Binary(
                        expr!(Identifier("i".into())),
                        token!(Plus),
                        expr!(Integer { int: 1 })
                    )),
                }),
            ))])),
        ));

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_trait_decl_success() {
        let input = "
trait TestTrait
  to_string(self) -> str
  method_with_default_impl(arg: i32) -> i32
    return arg
  to_int(self) -> i32";

        let parse_result = lex_and_parse(input);

        let expected = TraitDecl {
            trait_identifier: IdentifierToken::new_debug("TestTrait"),
            methods: vec![
                MethodDeclaration {
                    name: IdentifierToken::new_debug("to_string"),
                    self_: Some(MethodSelf { type_token: None }),
                    params: vec![],
                    return_ty: Some(IdentifierToken::new_debug("str")),
                    body: Block(vec![]),
                },
                MethodDeclaration {
                    name: IdentifierToken::new_debug("method_with_default_impl"),
                    self_: None,
                    params: vec![(
                        IdentifierToken::new_debug("arg"),
                        IdentifierToken::new_debug("i32"),
                    )],
                    return_ty: Some(IdentifierToken::new_debug("i32")),
                    body: Block(vec![StatementDecl(Ret(Some(dexpr!(Identifier(
                        "arg".into()
                    )))))]),
                },
                MethodDeclaration {
                    name: IdentifierToken::new_debug("to_int"),
                    self_: Some(MethodSelf { type_token: None }),
                    params: vec![],
                    return_ty: Some(IdentifierToken::new_debug("i32")),
                    body: Block(vec![]),
                },
            ],
        };

        assert_eq!(*parse_result.first().unwrap(), expected,)
    }

    #[test]
    fn test_if_stmt() {
        let input = "
if x != 3
    x = 10 / 2";
        let parse_result = lex_and_parse(input);

        let expected = StatementDecl(If(
            Expression::new_debug(Binary(
                expr!(Identifier("x".into())),
                token!(BangEqual),
                expr!(Integer { int: 3 }),
            )),
            Box::new(Block(vec![StatementDecl(ExpressionStmt(
                Expression::new_debug(Assign {
                    target: expr!(Identifier("x".into())),
                    value: expr!(Binary(
                        expr!(Integer { int: 10 }),
                        token!(Slash),
                        expr!(Integer { int: 2 }),
                    )),
                }),
            ))])),
            None,
        ));

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_if_stmt_with_else() {
        let input = "
if (x 
    != 3)
    x = 10 / 2
else
    x = 15 * 3
    ";
        let parse_result = lex_and_parse(input);

        let expected = StatementDecl(If(
            Expression::new_debug(Binary(
                expr!(Identifier("x".into())),
                token!(BangEqual),
                expr!(Integer { int: 3 }),
            )),
            Box::new(Block(vec![StatementDecl(ExpressionStmt(
                Expression::new_debug(Assign {
                    target: expr!(Identifier("x".into())),
                    value: expr!(Binary(
                        expr!(Integer { int: 10 }),
                        token!(Slash),
                        expr!(Integer { int: 2 }),
                    )),
                }),
            ))])),
            Some(Box::new(Block(vec![StatementDecl(ExpressionStmt(
                Expression::new_debug(Assign {
                    target: expr!(Identifier("x".into())),
                    value: expr!(Binary(
                        expr!(Integer { int: 15 }),
                        token!(Star),
                        expr!(Integer { int: 3 }),
                    )),
                }),
            ))]))),
        ));

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_var_decl() {
        let input = r#"var magic_number = "37""#;
        let parse_result = lex_and_parse(input);

        let expected = VarDecl(
            "magic_number".into(),
            Expression::new_debug(Str {
                string: "37".into(),
            }),
        );

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_instantiation() {
        let input = r#"
var my_struct = MyStruct {
    field1: 1 + 3,
    field2: "strings", // trailing comma required atm
}
        "#;

        let parse_result = lex_and_parse(input);

        let expected_struct = StructInit {
            name: expr!(Identifier("MyStruct".into())),
            values: vec![
                (
                    dexpr!(Identifier("field1".into())),
                    dexpr!(Binary(
                        expr!(Integer { int: 1 }),
                        token!(Plus),
                        expr!(Integer { int: 3 }),
                    )),
                ),
                (
                    dexpr!(Identifier("field2".into())),
                    dexpr!(Str {
                        string: "strings".into(),
                    }),
                ),
            ],
        };

        let expected = VarDecl("my_struct".into(), dexpr!(expected_struct));

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_access() {
        let input = "var chain_result = my_struct.method().chain()";

        let parse_result = lex_and_parse(input);

        let first_access = Access {
            expr: expr!(Identifier("my_struct".into())),
            name: expr!(Identifier("method".into())),
        };

        let first_call = Call(expr!(first_access), vec![]);

        let second_access = Access {
            expr: expr!(first_call),
            name: expr!(Identifier("chain".into())),
        };

        let second_call = Call(expr!(second_access), vec![]);

        let expected = VarDecl("chain_result".into(), dexpr!(second_call));

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_impl_block() {
        let input = "
impl MyStruct
    print_something()
        return 0

    is_positive(arg1: int) -> i32
        return 0
        ";

        let parse_result = lex_and_parse(input);

        let expected = Declaration::ImplDecl {
            struct_name: IdentifierToken::new_debug("MyStruct"),
            trait_name: None,
            methods: vec![
                MethodDeclaration {
                    name: IdentifierToken::new_debug("print_something"),
                    self_: None,
                    params: vec![],
                    return_ty: None,
                    body: Block(vec![StatementDecl(Ret(Some(dexpr!(Integer { int: 0 }))))]),
                },
                MethodDeclaration {
                    name: IdentifierToken::new_debug("is_positive"),
                    self_: None,
                    params: vec![(
                        IdentifierToken::new_debug("arg1"),
                        IdentifierToken::new_debug("int"),
                    )],
                    return_ty: Some(IdentifierToken::new_debug("i32")),
                    body: Block(vec![StatementDecl(Ret(Some(dexpr!(Integer { int: 0 }))))]),
                },
            ],
        };

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_impl_block_declaration_error() {
        let input = "
impl MyStruct

    is_positive(arg1: int) -> i32
    return 0
        ";

        let parse_err = lex_and_parse_err(input);

        assert_eq!(parse_err.message, "Expected method declaration");
    }
}
