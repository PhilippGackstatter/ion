extern crate ion;

use ion::compiler::Compiler;
use ion::lexer::Lexer;
use ion::parser::Parser;
use ion::util::pretty_print;

fn main() {
    let lexer = Lexer::new4();
    let mut parser = Parser::new(&lexer);
    let prog = parser.parse();
    println!("{:?}", prog);
    pretty_print(&prog);
    let mut compiler = Compiler::new();
    compiler.compile(&prog);
    println!("{}", compiler.chunk());
}
