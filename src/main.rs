mod lexer;
mod parser;
mod types;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new();
    let mut parser = Parser::new(&lexer);
    let expr = parser.parse();
    println!("{}", expr);
    pretty_print(0, false, &expr);
}

use types::Expression::{self, *};

fn pr(level: u32, is_child: bool, arg: &str) {
    for _ in 0..level {
        print!(" ");
    }
    if is_child {
        println!("└─ {}", arg);
    } else {
        println!("─ {}", arg);
    }
}

fn pretty_print(mut level: u32, is_child: bool, expr: &Expression) {
    match expr {
        Binary(lexpr, op, rexpr) => {
            pr(level, is_child, &format!("{:?}", op.kind));
            level += 2;
            pretty_print(level, true, rexpr);
            pretty_print(level, true, lexpr);
        }
        Unary(op, rexpr) => {
            pr(level, is_child, &format!("{:?}", op.kind));
            level += 2;
            pretty_print(level, true, rexpr);
        }
        Number(num) => {
            pr(level, is_child, &format!("{}", num));
        }
        Str(str_) => {
            pr(level, is_child, str_);
        }
        True => {
            pr(level, is_child, "true");
        }
        False => {
            pr(level, is_child, "false");
        }
    }
}
