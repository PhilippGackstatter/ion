extern crate ion;

use ion::compiler::Compiler;
use ion::lexer::Lexer;
use ion::parser::Parser;
use ion::util::pretty_print;

fn main() {
    let lexer = Lexer::new2();
    let mut parser = Parser::new(&lexer);
    let decl = parser.parse();
    println!("{:?}", decl);
    pretty_print(&decl);
    // let mut compiler = Compiler::new();
    // compiler.compile_(&decl);
    // println!("{}", compiler.chunk());
}
