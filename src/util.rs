use crate::types::Expression::{self, *};

pub fn pretty_print(expr: &Expression) {
    pretty_print_(0, false, expr);
}

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

fn pretty_print_(mut level: u32, is_child: bool, expr: &Expression) {
    match expr {
        Binary(lexpr, op, rexpr) => {
            pr(level, is_child, &format!("{:?}", op.kind));
            level += 2;
            pretty_print_(level, true, rexpr);
            pretty_print_(level, true, lexpr);
        }
        Unary(op, rexpr) => {
            pr(level, is_child, &format!("{:?}", op.kind));
            level += 2;
            pretty_print_(level, true, rexpr);
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
