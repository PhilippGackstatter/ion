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
            pr(level, true, id);
            pretty_print_expr(level, true, expr);
        }
        FnDecl(name, params, stmt) => {
            pr(
                level,
                is_child,
                &format!("fn {}({})", name, params.join(", ")),
            );
            level += 2;
            pretty_print_stmt(level, true, stmt);
        }
        StructDecl(name, fields) => {
            pr(level, is_child, &format!("struct {}", name.get_id()));
            level += 2;
            for (field, ty) in fields {
                pr(level, true, &format!("{}: {}", field.get_id(), ty.get_id()));
            }
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
        While(cond, body) => {
            pr(level, is_child, "while");
            level += 2;
            pretty_print_expr(level, true, cond);
            pretty_print_stmt(level, true, body);
        }
        Print(expr) => {
            pr(level, is_child, "print");
            level += 2;
            pretty_print_expr(level, true, expr);
        }
        Ret(expr) => {
            pr(level, is_child, "return");
            level += 2;
            match expr {
                Some(expr_) => pretty_print_expr(level, true, expr_),
                None => pr(level, true, "None"),
            }
        }
        Block(decls) => {
            pr(level, is_child, "Block");
            level += 2;
            for decl in decls.iter() {
                pretty_print_decl(level, true, decl);
            }
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
        Call(callee, params) => {
            pr(level, is_child, "call");
            level += 2;
            for param in params.iter() {
                pretty_print_expr(level, true, param);
            }
            pretty_print_expr(level, true, callee);
        }
        Assign(id, expr) => {
            pr(level, is_child, "Assign");
            level += 2;
            pr(level, is_child, id);
            pretty_print_expr(level, true, expr);
        }
        Integer(num, _) => {
            pr(level, is_child, &format!("{}", num));
        }
        Double(num, _) => {
            pr(level, is_child, &format!("{}", num));
        }
        Str(str_, _) => {
            pr(level, is_child, &format!(r#""{}""#, str_));
        }
        Identifier(str_) => {
            pr(level, is_child, str_);
        }
        True(_) => {
            pr(level, is_child, "true");
        }
        False(_) => {
            pr(level, is_child, "false");
        }
    }
}

pub fn lexit(program: String) {
    let mut lexer = crate::lexer::Lexer::new();
    lexer.lex(&program);
    lexer.print_tokens();
}

pub fn run(program: String, until: u8) {
    let mut lexer = crate::lexer::Lexer::new();
    lexer.lex(&program);
    lexer.print_tokens();
    if until == 1 {
        return;
    }
    let mut parser = crate::parser::Parser::new(&lexer);
    match parser.parse() {
        Ok(prog) => {
            crate::util::pretty_print(&prog);
            if until == 2 {
                return;
            }
            let mut checker = crate::type_checker::TypeChecker::new(&lexer.tokens);
            match checker.check(&prog) {
                Ok(()) => {
                    if until == 3 {
                        return;
                    }
                    let mut compiler: crate::compiler::Compiler = Default::default();
                    compiler.compile(&prog);

                    println!("{}", compiler.chunk());
                    println!("\nVirtual Machine");
                    println!("===============");
                    if until == 4 {
                        return;
                    }
                    let mut vm = crate::vm::VM::new();
                    vm.interpet(compiler.chunk());
                }
                Err(err) => {
                    print_error(&program, err.token, &err.message);
                }
            }
        }
        Err(parser_err) => {
            print_error(&program, parser_err.token, parser_err.message);
        }
    }
}

fn print_error(prog: &str, tk: crate::types::Token, msg: &str) {
    let mut newline_before_token = 0;
    // Initialized to prog len in case of last line
    let mut newline_after_token = prog.len();
    let mut line_count = 1;

    let mut break_on_next_newline = false;
    for (i, ch) in prog.bytes().enumerate() {
        if i >= (tk.offset as usize) {
            break_on_next_newline = true;
        }
        if ch == 10 {
            if break_on_next_newline {
                newline_after_token = i;
                break;
            } else {
                line_count += 1;
                newline_before_token = i;
            }
        }
    }

    println!();

    // Handle beginning of file
    if newline_before_token != 0 {
        newline_before_token += 1
    };

    let token = prog[newline_before_token..newline_after_token].to_owned();

    let token_start_index = (tk.offset as usize) - newline_before_token;
    let token_end_index = token_start_index + (tk.length as usize);
    let line_count_str = format!("{}", line_count);

    println!("{}: {}", line_count_str, token);

    for _ in 0..=(line_count_str.len() + 1) {
        print!(" ");
    }

    for i in 0..token_end_index {
        if i >= token_start_index {
            print!("^");
        } else {
            print!(" ");
        }
    }
    println!(" {}", msg);
}
