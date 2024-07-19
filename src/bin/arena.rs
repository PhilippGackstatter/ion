use ion::types::{Token, TokenKind};
use typed_arena::Arena;

fn main() {
    let mut parser = Parser { tokens_idx: 0 };
    let arenas = Arenas {
        statements: Arena::new(),
        expressions: Arena::new(),
    };

    let stmt = parser.parse_statement(&arenas);
    println!("{stmt:?}");
}

#[derive(Debug)]
enum Statement<'arena> {
    Print(&'arena Expression<'arena>),
}

#[derive(Debug)]
enum Expression<'arena> {
    Binary(
        &'arena Expression<'arena>,
        Token,
        &'arena Expression<'arena>,
    ),
    Integer(i32),
}

struct Arenas<'arena> {
    statements: Arena<Statement<'arena>>,
    expressions: Arena<Expression<'arena>>,
}

struct Parser {
    tokens_idx: usize,
}

impl Parser {
    fn parse_statement<'arena>(
        &mut self,
        arenas: &'arena Arenas<'arena>,
    ) -> &'arena mut Statement<'arena> {
        let expr = self.parse_expression(arenas);

        arenas.statements.alloc(Statement::Print(expr))
    }

    fn parse_expression<'arena>(
        &mut self,
        arenas: &'arena Arenas<'arena>,
    ) -> &'arena Expression<'arena> {
        let int1 = arenas.expressions.alloc(Expression::Integer(39));
        let int2 = arenas.expressions.alloc(Expression::Integer(39));
        let plus = arenas.expressions.alloc(Expression::Binary(
            int1,
            Token::new(0, 0, TokenKind::Plus),
            int2,
        ));
        self.advance();
        plus
    }

    fn advance(&mut self) {
        self.tokens_idx += 1;
    }
}
