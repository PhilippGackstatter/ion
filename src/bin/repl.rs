extern crate ion;
// use std::env;
use std::io;
use std::io::prelude::*;

fn main() {
    // Specify anything as the 1st arg to enable debug mode
    // let debug_enabled = if let Some(_) = env::args().nth(1) {
    //     true
    // } else {
    //     false
    // };

    println!("ion v0.1.0");
    loop {
        print!(">> ");
        io::stdout().flush().expect("Could not flush stdout");

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                run(input);
            }
            Err(error) => println!("error: {}", error),
        }
    }
}

// fn lexme(program: String) {
//     let mut lexer = ion::lexer::Lexer::new();
//     lexer.lex(program);
//     lexer.print_tokens();
// }

fn run(program: String) {
    let mut lexer = ion::lexer::Lexer::new();
    lexer.lex(program);
    lexer.print_tokens();
    let mut parser = ion::parser::Parser::new(&lexer);
    let prog = parser.parse();
    ion::util::pretty_print(&prog);
    let mut compiler = ion::compiler::Compiler::new();
    compiler.compile(&prog);
    println!("{}", compiler.chunk());
    let mut vm = ion::vm::VM::new();
    vm.interpet(compiler.chunk());
}
