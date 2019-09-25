use crate::types::{
    Declaration::{self, *},
    Expression::{self, *},
    Program,
    Statement::{self, *},
};

pub fn pretty_print(prog: &Program) {
    println!("\nAbstract Syntax Tree");
    println!("====================");
    for decl in prog.iter() {
        pretty_print_decl(0, false, decl);
    }
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

fn pretty_print_decl(mut level: u32, is_child: bool, decl: &Declaration) {
    match decl {
        StatementDecl(stmt) => pretty_print_stmt(level, is_child, stmt),
        VarDecl(id, expr) => {
            pr(level, is_child, "var");
            level += 2;
            pr(level, true, &format!("{}", id));
            pretty_print_expr(level, true, expr);
        }
    }
}

fn pretty_print_stmt(mut level: u32, is_child: bool, stmt: &Statement) {
    match stmt {
        ExpressionStmt(expr) => pretty_print_expr(level, is_child, expr),
        If(cond, then_stmt, else_stmt) => {
            pr(level, is_child, "if");
            level += 2;
            pretty_print_expr(level, true, cond);
            pretty_print_stmt(level, true, then_stmt);
            if let Some(else_) = else_stmt {
                pretty_print_stmt(level, true, else_);
            }
        }
        Print(expr) => {
            pr(level, is_child, "print");
            level += 2;
            pretty_print_expr(level, true, expr);
        }
    }
}

fn pretty_print_expr(mut level: u32, is_child: bool, expr: &Expression) {
    match expr {
        Binary(lexpr, op, rexpr) => {
            pr(level, is_child, &format!("{:?}", op.kind));
            level += 2;
            pretty_print_expr(level, true, rexpr);
            pretty_print_expr(level, true, lexpr);
        }
        Unary(op, rexpr) => {
            pr(level, is_child, &format!("{:?}", op.kind));
            level += 2;
            pretty_print_expr(level, true, rexpr);
        }
        Number(num) => {
            pr(level, is_child, &format!("{}", num));
        }
        Str(str_) => {
            pr(level, is_child, &format!(r#""{}""#, str_));
        }
        Identifier(str_) => {
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
