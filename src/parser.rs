use crate::lexer::Lexer;
use crate::types::Token;
use crate::types::TokenKind::{self, *};
use crate::types::{
    CompileError,
    Declaration::{self, *},
    Expression,
    ExpressionKind::*,
    Program,
    Statement::{self, *},
};
use std::ops::Range;

type StatementResult = Result<Statement, CompileError>;
type ExpressionResult = Result<Expression, CompileError>;
type DeclarationResult = Result<Declaration, CompileError>;

pub struct Parser<'a> {
    lexer: &'a Lexer,
    current: usize,
    wstack: Vec<u8>,
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
                if self.is_function_declaration() {
                    self.function_declaration()
                } else {
                    Ok(StatementDecl(self.statement()?))
                }
            }
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

    fn struct_declaration(&mut self) -> DeclarationResult {
        self.advance();

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected an identifier as struct name.",
            ));
        }
        let name = self.advance().clone();

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

                let name_token = self.advance().clone();

                self.consume(Colon, "Expected ':' after name.")?;

                if !self.peek().is_id_token() {
                    return Err(self.error(
                        self.peek().clone().into(),
                        "Expected a type identifier after field name.",
                    ));
                }

                let type_token = self.advance().clone();

                fields.push((name_token, type_token));

                self.expect_newline()?;
            }

            self.pop_indentation();
        }

        Ok(StructDecl(name, fields))
    }

    fn impl_declaration(&mut self) -> DeclarationResult {
        self.advance();

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone().into(),
                "Expected an identifier as struct name.",
            ));
        }
        let name = self.advance().clone();

        self.expect_newline()?;

        let mut methods = Vec::new();

        if self.set_next_indentation()? {
            while self.has_same_indentation()? {
                if self.is_function_declaration() {
                    methods.push(self.function_declaration()?);
                } else {
                    return Err(
                        self.error(self.peek().clone().into(), "Expected function declaration")
                    );
                }
            }

            self.pop_indentation();
        }

        Ok(ImplDecl {
            struct_name: name,
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

    fn is_function_declaration(&mut self) -> bool {
        let current = self.current;

        let mut is_fn_decl = false;

        while self.peek().kind != EndOfFile {
            match self.advance().kind {
                NewLine => {
                    if let WhiteSpace(indent) = self.peek().kind {
                        if self.is_indented(indent) {
                            is_fn_decl = true;
                        }
                    }
                    break;
                }

                StructToken => break,
                ImplToken => break,

                _ => (),
            }
        }

        self.current = current;

        is_fn_decl
    }

    fn function_declaration(&mut self) -> DeclarationResult {
        let id = self.advance().get_id();

        self.consume(LeftParen, "Expected '(' after function name.")?;

        let mut params = Vec::new();

        if self.peek().kind != RightParen {
            // Consume comma after parameter
            while self.peek().kind != RightParen {
                let name_token = self.advance().clone();

                self.consume(Colon, "Expected ':' for type annotation.")?;

                if !self.peek().is_id_token() {
                    return Err(self.error(
                        self.peek().clone().into(),
                        "Expected type identifier after parameter.",
                    ));
                }

                let type_token = self.advance().clone();

                params.push((name_token, type_token));

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

        self.consume(RightParen, "Expected ')' after function parameters.")?;

        let return_token = if self.match_(Arrow) {
            Some(self.advance().clone())
        } else {
            None
        };

        let mut body = self.block()?;

        // Add a return statement if it does not exist
        if let Block(decls) = &mut body {
            if let Some(StatementDecl(Ret(_))) = &decls.last() {
            } else {
                decls.push(StatementDecl(Ret(None)));
            }
        }
        Ok(FnDecl(id, params, return_token, body))
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
        let unexpected_eof = false;

        if self.set_next_indentation()? {
            while self.has_same_indentation()? {
                decls.push(self.declaration()?);
            }

            self.pop_indentation();
        }

        if unexpected_eof {
            Err(self.error(
                self.previous().clone().into(),
                "Unclosed block, missing a '}'.",
            ))
        } else {
            Ok(Block(decls))
        }
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
        if self.match_(Semicolon) {
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
        self.consume(LeftParen, "Expected '(' after while.")?;
        let condition_expr = self.expression()?;
        self.consume(RightParen, "Expected ')' after condition.")?;
        let body = self.statement()?;

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
                (operator.offset as usize)..lexpr.tokens.end,
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
                // TODO: Fix case when struct has size 0
                let last_token_end = fields[fields.len() - 1].1.tokens.end;
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
            _ => {
                if self.match_(LeftParen) {
                    let expr = self.expression()?;
                    self.consume(RightParen, "Expected '('.")?;
                    Ok(expr)
                } else {
                    Err(self.error(self.peek().clone().into(), "Expect expression."))
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
            if *self.wstack.last().unwrap() < indent {
                self.wstack.push(indent);
                Ok(true)
            } else {
                Err(self.error(self.peek().clone().into(), "Expected indentation"))
            }
        } else {
            Ok(false)
        }
    }

    fn has_same_indentation(&mut self) -> Result<bool, CompileError> {
        if let WhiteSpace(indent) = self.peek().kind {
            let mut rev_iter = self.wstack.iter().rev();
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
                    self.wstack.last().unwrap(),
                    indent
                );
                Err(self.error(self.peek().clone().into(), &err_msg))
            }
        } else {
            Ok(false)
        }
    }

    fn is_indented(&self, indent: u8) -> bool {
        *self.wstack.last().unwrap() < indent
    }

    fn expect_newline(&mut self) -> Result<(), CompileError> {
        match self.peek().kind {
            EndOfFile => Ok(()),
            _ => self.consume(NewLine, "Expected newline"),
        }
    }

    fn pop_indentation(&mut self) {
        if self.wstack.len() > 1 {
            self.wstack.pop();
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
            wstack: vec![0],
        }
    }
}

#[cfg(test)]
mod tests {
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
        lex_and_parse_impl(input).unwrap()
    }

    fn lex_and_parse_err(input: &str) -> CompileError {
        lex_and_parse_impl(input).unwrap_err()
    }

    fn lex_and_parse_impl(input: &str) -> Result<Program, CompileError> {
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let mut parser = Parser::new(&lexer);
        parser.parse()
    }

    #[test]
    fn test_multiplication() {
        // Test 9 + 1 / 4
        let lexer = Lexer::new_from_tokenkind(vec![
            Num(9),
            Plus,
            Num(1),
            Slash,
            Num(4),
            Semicolon,
            EndOfFile,
        ]);
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
        let lexer = Lexer::new_from_tokenkind(vec![Bang, Bang, FalseToken, Semicolon, EndOfFile]);
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
    fn test_function_decl() {
        let input = "fn foo(arg1: str, arg2: bool) {}";
        let parse_result = lex_and_parse(&input);

        let expected = FnDecl(
            "foo".into(),
            vec![
                (
                    token!(IdToken("arg1".into())),
                    token!(IdToken("str".into())),
                ),
                (
                    token!(IdToken("arg2".into())),
                    token!(IdToken("bool".into())),
                ),
            ],
            None,
            Block(vec![StatementDecl(Ret(None))]),
        );

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_decl() {
        let input = "
struct MyStruct
    field1: str
    field2: bool
        ";

        let parse_result = lex_and_parse(&input);

        let expected = StructDecl(
            token!(IdToken("MyStruct".into())),
            vec![
                (
                    token!(IdToken("field1".into())),
                    token!(IdToken("str".into())),
                ),
                (
                    token!(IdToken("field2".into())),
                    token!(IdToken("bool".into())),
                ),
            ],
        );

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

        let parse_result = lex_and_parse(&input);

        let expected = StructDecl(
            token!(IdToken("MyStruct".into())),
            vec![
                (
                    token!(IdToken("field1".into())),
                    token!(IdToken("str".into())),
                ),
                (
                    token!(IdToken("field2".into())),
                    token!(IdToken("bool".into())),
                ),
            ],
        );

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_decl_no_fields() {
        let input = "
// Various amount of whitespace
struct MyStruct
            
   
        ";

        let parse_result = lex_and_parse(&input);

        let expected = StructDecl(token!(IdToken("MyStruct".into())), vec![]);

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_struct_decl_missing_indentation_error() {
        let input = "struct MyStruct\nfield: type";

        let parse_err = lex_and_parse_err(&input);

        assert_eq!(parse_err.message, "Expected fields to be indented");
    }

    #[test]
    fn test_fn_decl() {
        let input = "
foo(arg: i32) -> i32
    bar()
    x = 10 / 2
    print 3
    return 5";

        let parse_result = lex_and_parse(&input);

        let bar_call = Call(expr!(Identifier("bar".into())), vec![]);

        let expected = FnDecl(
            "foo".into(),
            vec![(token!(IdToken("arg".into())), token!(IdToken("i32".into())))],
            Some(token!(IdToken("i32".into()))),
            Block(vec![
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
        );
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

        let parse_result = lex_and_parse(&input);

        let expected = FnDecl(
            "long_function_name".into(),
            vec![
                (token!(IdToken("arg".into())), token!(IdToken("i32".into()))),
                (token!(IdToken("oth".into())), token!(IdToken("str".into()))),
            ],
            Some(token!(IdToken("i32".into()))),
            Block(vec![StatementDecl(Ret(Some(dexpr!(Integer { int: 5 }))))]),
        );
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

        let parse_err = lex_and_parse_err(&input);

        assert_eq!(parse_err.message, "Expected indentation of 4 but got 2");
    }

    #[test]
    fn test_while_stmt() {
        let input = "while (i < 3) i = i + 1;";
        let parse_result = lex_and_parse(&input);

        let expected = StatementDecl(While(
            Expression::new_debug(Binary(
                expr!(Identifier("i".into())),
                token!(Less),
                expr!(Integer { int: 3 }),
            )),
            Box::new(ExpressionStmt(Expression::new_debug(Assign {
                target: expr!(Identifier("i".into())),
                value: expr!(Binary(
                    expr!(Identifier("i".into())),
                    token!(Plus),
                    expr!(Integer { int: 1 }),
                )),
            }))),
        ));

        assert_eq!(*parse_result.first().unwrap(), expected)
    }

    #[test]
    fn test_if_stmt() {
        let input = "
if x != 3
    x = 10 / 2";
        let parse_result = lex_and_parse(&input);

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
        let parse_result = lex_and_parse(&input);

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
        let parse_result = lex_and_parse(&input);

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

        let parse_result = lex_and_parse(&input);

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

        let parse_result = lex_and_parse(&input);

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

        let parse_result = lex_and_parse(&input);

        let expected = Declaration::ImplDecl {
            struct_name: token!(IdToken("MyStruct".into())),
            methods: vec![
                FnDecl(
                    "print_something".into(),
                    vec![],
                    None,
                    Block(vec![StatementDecl(Ret(Some(dexpr!(Integer { int: 0 }))))]),
                ),
                FnDecl(
                    "is_positive".into(),
                    vec![(
                        token!(IdToken("arg1".into())),
                        token!(IdToken("int".into())),
                    )],
                    Some(token!(IdToken("i32".into()))),
                    Block(vec![StatementDecl(Ret(Some(dexpr!(Integer { int: 0 }))))]),
                ),
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

        let parse_err = lex_and_parse_err(&input);

        assert_eq!(parse_err.message, "Expected function declaration");
    }
}
