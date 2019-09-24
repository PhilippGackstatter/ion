extern crate ion;

use ion::compiler::Compiler;
use ion::lexer::Lexer;
use ion::parser::Parser;
use ion::util::pretty_print;
use ion::vm::VM;

fn main() {
    let lexer = Lexer::new3();
    let mut parser = Parser::new(&lexer);
    let decl = parser.parse();
    // println!("{:?}", decl);
    pretty_print(&decl);
    let mut compiler = Compiler::new();
    compiler.compile(&decl);
    println!("{}", compiler.chunk());
    let mut vm = VM::new();
    vm.interpet(compiler.chunk());
}
