extern crate ion;

use ion::compiler::Compiler;
use ion::lexer::Lexer;
use ion::parser::Parser;
use ion::util::pretty_print;

fn main() {
    let lexer = Lexer::new();
    let mut parser = Parser::new(&lexer);
    let expr = parser.parse();
    println!("{}", expr);
    pretty_print(&expr);
    let mut compiler = Compiler::new();
    compiler.compile_(&expr);
    println!("{}", compiler.chunk());
}
