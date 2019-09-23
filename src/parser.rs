use crate::lexer::Lexer;
use crate::types::Expression::{self, *};
use crate::types::Token;
use crate::types::TokenKind::{self, *};

// TODO: Should be expanded to include the line and column directly,
// since the parser (and thus the lexer and source code) are no longer accessible
#[derive(Debug)]
struct ParserError {
    token: Token,
    message: &'static str,
}

pub struct Parser<'a> {
    lexer: &'a Lexer,
    current: usize,
}

impl<'a> Parser<'a> {
    fn expression(&mut self) -> Expression {
        self.equality()
    }

    fn equality(&mut self) -> Expression {
        // equality → comparison ( ( "!=" | "==" ) comparison )* ;
        let mut expr = self.comparison();
        while self.match_(BangEqual) || self.match_(EqualEqual) {
            let operator = self.previous().clone();
            let right = self.comparison();
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        expr
    }

    fn comparison(&mut self) -> Expression {
        // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
        let mut expr = self.addition();
        while self.match_(Greater)
            || self.match_(GreaterEqual)
            || self.match_(Less)
            || self.match_(LessEqual)
        {
            let operator = self.previous().clone();
            let right = self.addition();
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        expr
    }

    fn addition(&mut self) -> Expression {
        let mut expr = self.multiplication();
        while self.match_(Plus) || self.match_(Minus) {
            let operator = self.previous().clone();
            let right = self.multiplication();
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        expr
    }

    fn multiplication(&mut self) -> Expression {
        let mut expr = self.unary();
        while self.match_(Slash) || self.match_(Star) {
            let operator = self.previous().clone();
            let right = self.unary();
            expr = Binary(Box::new(expr), operator, Box::new(right));
        }
        expr
    }

    fn unary(&mut self) -> Expression {
        // unary → ( "!" | "-" ) unary
        //         | primary ;
        if self.match_(Bang) || self.match_(Minus) {
            let operator = self.previous().clone();
            let lexpr = self.unary();
            return Unary(operator, Box::new(lexpr));
        }

        self.primary()
    }

    fn primary(&mut self) -> Expression {
        // primary → NUMBER | STRING | "false" | "true" | "nil"
        //           | "(" expression ")" ;
        match &self.peek().kind {
            True_ => {
                self.advance();
                True
            }
            False_ => {
                self.advance();
                False
            }
            Num(int) => {
                let num = Number(*int);
                self.advance();
                num
            }
            String_(str_) => {
                let string = Str(str_.clone());
                self.advance();
                string
            }
            _ => {
                self.consume(LeftParen, "Expected '('.").unwrap();
                let expr = self.expression();
                self.consume(RightParen, "Expected '('.").unwrap();
                expr
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

    // Misc

    pub fn new(lexer: &'a Lexer) -> Self {
        Parser { lexer, current: 0 }
    }

    pub fn parse(&mut self) -> Expression {
        self.expression()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiplication() {
        // Test 9 + 1 / 4
        let lexer = Lexer::new_from_tokens(vec![
            Token::new_debug(Num(9)),
            Token::new_debug(Plus),
            Token::new_debug(Num(1)),
            Token::new_debug(Slash),
            Token::new_debug(Num(4)),
            Token::new_debug(EndOfFile),
        ]);
        let mut parser = Parser::new(&lexer);
        let res = parser.parse();
        assert_eq!(
            res,
            Binary(
                Box::new(Number(9)),
                Token::new_debug(Plus),
                Box::new(Binary(
                    Box::new(Number(1)),
                    Token::new_debug(Slash),
                    Box::new(Number(4))
                ))
            )
        );
    }

    #[test]
    fn test_unary() {
        // Test "!!false"
        let lexer = Lexer::new_from_tokens(vec![
            Token::new_debug(Bang),
            Token::new_debug(Bang),
            Token::new_debug(False_),
            Token::new_debug(EndOfFile),
        ]);
        let mut parser = Parser::new(&lexer);
        let res = parser.parse();
        assert_eq!(
            res,
            Unary(
                Token::new_debug(Bang),
                Box::new(Unary(Token::new_debug(Bang), Box::new(False)))
            )
        );
    }

}
