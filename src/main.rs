mod types;
mod parser;
mod lexer;

use parser::Parser;
use lexer::Lexer;

fn main() {
    let lexer = Lexer::new();
    let mut parser = Parser::new(&lexer);
    let expr = parser.parse();
    println!("{}", expr);
}
