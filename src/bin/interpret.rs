extern crate ion;

use ion::compiler::Compiler;
use ion::lexer::Lexer;
use ion::parser::Parser;
use ion::util::pretty_print;
use ion::vm::VM;

fn main() {
    let lexer = Lexer::new3();
    let mut parser = Parser::new(&lexer);
    let prog = parser.parse();
    // println!("{:?}", decl);
    pretty_print(&prog);
    let mut compiler = Compiler::new();
    compiler.compile(&prog);
    println!("{}", compiler.chunk());
    let mut vm = VM::new();
    vm.interpet(compiler.chunk());
}
