use crate::lexer::Lexer;
use crate::types::Token;
use crate::types::TokenKind::{self, *};
use crate::types::{
    Declaration::{self, *},
    Expression,
    ExpressionKind::*,
    Program,
    Statement::{self, *},
};
use std::ops::Range;

// TODO: Should be expanded to include the line and column directly,
// since the parser (and thus the lexer and source code) are no longer accessible
#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub message: &'static str,
}

type StatementResult = Result<Statement, ParserError>;
type ExpressionResult = Result<Expression, ParserError>;
type DeclarationResult = Result<Declaration, ParserError>;

pub struct Parser<'a> {
    lexer: &'a Lexer,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        self.program()
    }

    fn program(&mut self) -> Result<Program, ParserError> {
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
            Fun => self.function_declaration(),
            StructToken => self.struct_declaration(),
            _ => Ok(StatementDecl(self.statement()?)),
        }
    }

    fn struct_declaration(&mut self) -> DeclarationResult {
        self.advance();

        if !self.peek().is_id_token() {
            return Err(self.error(
                self.peek().clone(),
                "Expected an identifier as struct name.",
            ));
        }
        let name = self.advance().clone();

        self.consume(LeftBrace, "Expected '{' after struct.")?;
        let mut fields = Vec::new();

        while self.peek().kind != RightBrace {
            if !self.peek().is_id_token() {
                return Err(
                    self.error(self.peek().clone(), "Expected an identifier as field name.")
                );
            }

            let name_token = self.advance().clone();

            self.consume(Colon, "Expected ':' after name.")?;

            if !self.peek().is_id_token() {
                return Err(self.error(
                    self.peek().clone(),
                    "Expected a type identifier after field name.",
                ));
            }

            let type_token = self.advance().clone();

            if self.peek().kind != RightBrace {
                if self.peek().kind == Comma {
                    self.advance();
                } else {
                    return Err(self.error(self.peek().clone(), "Expected a ',' inbetween fields."));
                }
            } else if self.peek().kind == Comma {
                // Allow trailing comma
                self.advance();
            }

            fields.push((name_token, type_token));
        }
        // Consume }
        self.advance();

        Ok(StructDecl(name, fields))
    }

    fn variable_declaration(&mut self) -> DeclarationResult {
        // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
        self.advance();
        let id = if let IdToken(id_) = &self.advance().kind {
            Ok(id_.clone())
        } else {
            Err(self.error(
                self.peek().clone(),
                "Expected an identifier after variable declaration.",
            ))
        }?;

        self.consume(Equal, "Expected '=' after variable declaration.")?;
        let expr = self.expression()?;
        self.consume(Semicolon, "Expected ';' after expression.")?;
        Ok(VarDecl(id.clone(), expr))
    }

    fn function_declaration(&mut self) -> DeclarationResult {
        self.advance();

        let id = if let IdToken(id_) = &self.advance().kind {
            Ok(id_.clone())
        } else {
            Err(self.error(
                self.peek().clone(),
                "Expected an identifier after function declaration.",
            ))
        }?;

        self.consume(LeftParen, "Expected '(' after function name.")?;

        let mut params = Vec::new();

        if self.peek().kind != RightParen {
            // Consume comma after parameter
            while self.peek().kind != RightParen {
                let name_token = self.advance().clone();

                self.consume(Colon, "Expected ':' for type annotation.")?;

                if !self.peek().is_id_token() {
                    return Err(self.error(
                        self.peek().clone(),
                        "Expected type identifier after parameter.",
                    ));
                }

                let type_token = self.advance().clone();

                params.push((name_token, type_token));

                if !self.match_(Comma) && self.peek().kind != RightParen {
                    return Err(
                        self.error(self.peek().clone(), "Expected ')' or ',' after parameter.")
                    );
                }
            }

            if params.is_empty() {
                return Err(self.error(self.peek().clone(), "Expected identifier."));
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
        self.advance();
        let mut decls = Vec::new();
        let mut unexpected_eof = false;

        while !self.match_(RightBrace) {
            if self.is_at_end() {
                unexpected_eof = true;
                break;
            }
            decls.push(self.declaration()?);
        }

        if unexpected_eof {
            Err(self.error(self.previous().clone(), "Unclosed block, missing a '}'."))
        } else {
            Ok(Block(decls))
        }
    }

    fn expression_statement(&mut self) -> StatementResult {
        let res = self.expression()?;
        self.consume(Semicolon, "Expected ';' after expression.")?;
        Ok(ExpressionStmt(res))
    }

    fn print_statement(&mut self) -> StatementResult {
        self.advance();
        let expr = self.expression()?;
        self.consume(Semicolon, "Expected ';' after expression.")?;
        Ok(Print(expr))
    }

    fn return_statement(&mut self) -> StatementResult {
        self.advance();
        if self.match_(Semicolon) {
            Ok(Ret(None))
        } else {
            let expr = self.expression()?;
            self.consume(Semicolon, "Expected ';' after return statement.")?;
            Ok(Ret(Some(expr)))
        }
    }

    fn if_statement(&mut self) -> StatementResult {
        self.advance();
        self.consume(LeftParen, "Expected '(' after if.")?;
        let condition_expr = self.expression()?;
        self.consume(RightParen, "Expected ')' after condition.")?;
        let if_stmt = self.statement()?;

        let else_branch = if self.match_(ElseToken) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(If(condition_expr, Box::new(if_stmt), else_branch))
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
            let equals = self.previous().clone();
            if let Identifier(id) = &left_hand?.kind {
                let value = self.assignment()?;
                return Ok(Expression::new(
                    equals.into(),
                    Assign(id.clone(), Box::new(value)),
                ));
            } else {
                return Err(self.error(equals, "Invalid assignment target."));
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
        let mut expr = self.primary()?;

        loop {
            if self.match_(LeftParen) {
                expr = self.finish_call(expr)?;
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
                    Err(self.error(self.peek().clone(), "Expect expression."))
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

    // Errors

    fn consume(&mut self, token: TokenKind, error_msg: &'static str) -> Result<(), ParserError> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(self.peek().clone(), error_msg))
        }
    }

    fn error(&self, token: Token, message: &'static str) -> ParserError {
        ParserError { token, message }
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
        Parser { lexer, current: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(unused_macros)]
    macro_rules! expr {
    [$exp:expr] => {
            {
                Box::new(Expression::new_debug($exp))
            }
        };
    }

    #[allow(unused_macros)]
    macro_rules! token {
        [$token:expr] => {
            {
                Token::new_debug($token)
            }
        };
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

}
