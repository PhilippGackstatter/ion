extern crate ion;

use ion::lexer::Lexer;
use ion::parser::Parser;
use ion::util::pretty_print;

fn main() {
    let lexer = Lexer::new4();
    let mut parser = Parser::new(&lexer);
    let decl = parser.parse();
    println!("{:?}", decl);
    pretty_print(&decl);
}
