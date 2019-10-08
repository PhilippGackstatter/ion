use crate::lexer::Lexer;
use crate::types::Token;
use crate::types::TokenKind::{self, *};
use crate::types::{
    Declaration::{self, *},
    Expression::{self, *},
    Program,
    Statement::{self, *},
};

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
            _ => Ok(StatementDecl(self.statement()?)),
        }
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
            while let IdToken(name) = &self.peek().kind {
                params.push(name.clone());
                self.advance();

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

        let mut body = self.block()?;

        // Add a return statement if it does not exist
        if let Block(decls) = &mut body {
            if let StatementDecl(Ret(_)) = &decls[decls.len() - 1] {
            } else {
                decls.push(StatementDecl(Ret(None)));
            }
        }

        Ok(FnDecl(id, params, body))
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
            let equals = self.previous();
            if let Identifier(id) = left_hand? {
                let value = self.assignment()?;
                return Ok(Assign(id, Box::new(value)));
            } else {
                return Err(self.error(equals.clone(), "Invalid assignment target."));
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
            expr = Binary(Box::new(expr), operator, Box::new(right));
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
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn addition(&mut self) -> ExpressionResult {
        // addition → multiplication ( ( "-" | "+" ) multiplication )* ;
        let mut expr = self.multiplication()?;
        while self.match_(Plus) || self.match_(Minus) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> ExpressionResult {
        // multiplication → unary ( ( "/" | "*" ) unary )* ;
        let mut expr = self.unary()?;
        while self.match_(Slash) || self.match_(Star) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExpressionResult {
        // unary → ( "!" | "-" ) unary
        //         | primary ;
        if self.match_(Bang) || self.match_(Minus) {
            let operator = self.previous().clone();
            let lexpr = self.unary()?;
            Ok(Unary(operator, Box::new(lexpr)))
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

        Ok(Call(Box::new(expr), params))
    }

    fn primary(&mut self) -> ExpressionResult {
        // primary → NUMBER | STRING | "false" | "true" | "nil"
        //           | "(" expression ")" ;
        match &self.peek().kind {
            TrueToken => {
                self.advance();
                Ok(True)
            }
            FalseToken => {
                self.advance();
                Ok(False)
            }
            Num(int) => {
                let num = Integer(*int, self.current);
                self.advance();
                Ok(num)
            }
            FloatNum(float) => {
                let num = Double(*float, self.current);
                self.advance();
                Ok(num)
            }
            String_(str_) => {
                let string = Str(str_.clone(), self.current);
                self.advance();
                Ok(string)
            }
            IdToken(str_) => {
                let id = Identifier(str_.clone());
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
            // println!("Token {:?} matched", self.peek());
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
            assert_eq!(
                *expr,
                Binary(
                    Box::new(Integer(9, 0)),
                    Token::new_debug(Plus),
                    Box::new(Binary(
                        Box::new(Integer(1, 0)),
                        Token::new_debug(Slash),
                        Box::new(Integer(4, 0))
                    ))
                )
            );
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
                Unary(
                    Token::new_debug(Bang),
                    Box::new(Unary(Token::new_debug(Bang), Box::new(False)))
                )
            );
        } else {
            panic!();
        }
    }

}
