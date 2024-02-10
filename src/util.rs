use argparse::{ArgumentParser, Store, StoreTrue};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::types::{
    Declaration::{self, *},
    Expression,
    ExpressionKind::*,
    MethodSelf, Program,
    Statement::{self, *},
    Token,
};
use std::fmt::{self, Debug, Formatter};

pub struct Options {
    // Whether to debug print the following data structures
    pub tokens: bool,
    pub ast: bool,
    pub symbols: bool,
    pub chunk: bool,
    pub vm: bool,
    /// Until what stage to run the compiler
    pub until: u8,
}

impl Options {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Options {
            tokens: false,
            ast: false,
            symbols: false,
            chunk: false,
            vm: false,
            until: 5,
        }
    }
}

pub fn get_cli_parser(opt: &mut Options) -> ArgumentParser {
    let mut parser = ArgumentParser::new();

    parser.set_description("ion language");
    parser.refer(&mut opt.tokens).add_option(
        &["-t", "--tokens"],
        StoreTrue,
        "Print the tokens produced by the Lexer.",
    );

    parser.refer(&mut opt.ast).add_option(
        &["-a", "--ast"],
        StoreTrue,
        "Print the abstract syntax tree produced by the Parser.",
    );

    parser.refer(&mut opt.symbols).add_option(
        &["-s", "--symbols"],
        StoreTrue,
        "Print the symbol table produced by the Type Checker.",
    );

    parser.refer(&mut opt.chunk).add_option(
        &["-c", "--chunk"],
        StoreTrue,
        "Print the Chunk (Constants + Bytecode) produced by the Compiler.",
    );

    parser.refer(&mut opt.vm).add_option(
        &["-v", "--vm"],
        StoreTrue,
        "Print the execution trace by the VM.",
    );

    parser
        .refer(&mut opt.until)
        .add_option(&["-u", "--until"], Store, "Until what stage to run: 1 Lexer, 2 Parser, 3 Type Checker, 4 Compiler, 5 Virtual Machine.");

    parser
}

fn write(f: &mut Formatter<'_>, level: u32, is_child: bool, arg: &str) -> fmt::Result {
    for _ in 0..level {
        write!(f, " ")?;
    }
    if is_child {
        writeln!(f, "└─ {}", arg)
    } else {
        writeln!(f, "─ {}", arg)
    }
}

pub fn pretty_write_expr(
    f: &mut Formatter<'_>,
    mut level: u32,
    is_child: bool,
    expr: &Expression,
) -> fmt::Result {
    match &expr.kind {
        Binary(lexpr, op, rexpr) => {
            write(f, level, is_child, &format!("{:?}", op))?;
            level += 2;
            pretty_write_expr(f, level, true, rexpr)?;
            pretty_write_expr(f, level, true, lexpr)
        }
        Unary(op, rexpr) => {
            write(f, level, is_child, &format!("{:?}", op.kind))?;
            level += 2;
            pretty_write_expr(f, level, true, rexpr)
        }
        Call(callee, params) => {
            write(f, level, is_child, "call")?;
            level += 2;
            for param in params.iter() {
                pretty_write_expr(f, level, true, param)?;
            }
            pretty_write_expr(f, level, true, callee)
        }
        Assign { target, value } => {
            write(f, level, is_child, "Assign")?;
            level += 2;
            pretty_write_expr(f, level, true, target)?;
            pretty_write_expr(f, level, true, value)
        }
        StructInit { name, values } => {
            write(f, level, is_child, &format!("Struct {}", name.get_id()))?;
            level += 2;
            for (name, value) in values.iter() {
                let mut temp = level;
                pretty_write_expr(f, temp, true, name)?;
                temp += 2;
                pretty_write_expr(f, temp, true, value)?;
            }
            Ok(())
        }
        Access { expr, name } => {
            write(f, level, is_child, "Access")?;
            level += 2;
            pretty_write_expr(f, level, is_child, expr)?;
            pretty_write_expr(f, level, is_child, name)
        }
        Integer { int } => write(f, level, is_child, &format!("{}", int)),
        Double { float } => write(f, level, is_child, &format!("{}", float)),
        Str { string } => write(f, level, is_child, &format!("\"{}\"", string)),
        Identifier(str_) => write(f, level, is_child, str_),
        True { .. } => write(f, level, is_child, "true"),
        False { .. } => write(f, level, is_child, "false"),
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        pretty_write_expr(f, 0, false, self)
    }
}

impl Debug for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        pretty_write_stmt(f, 0, false, self)
    }
}

impl Debug for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        pretty_write_decl(f, 0, false, self)
    }
}

pub fn pretty_print(prog: &Program) {
    println!("\nAbstract Syntax Tree");
    println!("====================");
    for decl in prog.iter() {
        println!("{:?}", decl);
    }
}

fn pretty_write_callable(
    f: &mut Formatter<'_>,
    mut level: u32,
    is_child: bool,
    name: &str,
    self_: Option<&MethodSelf>,
    params: &[(Token, Token)],
    return_ty: Option<Token>,
    body: &Statement,
) -> fmt::Result {
    let ret = if return_ty.is_some() {
        return_ty.as_ref().unwrap().get_id()
    } else {
        "void".to_owned()
    };

    let mut params_str: String = match self_ {
        Some(method_self) => match &method_self.type_token {
            Some(type_token) => format!("self: {}, ", type_token.get_id()),
            None => "self, ".to_owned(),
        },
        None => "".to_owned(),
    };

    params_str.push_str(
        &params
            .iter()
            .map(|p| format!("{}: {}", p.0.get_id(), p.1.get_id()))
            .collect::<Vec<String>>()
            .join(", "),
    );

    write(
        f,
        level,
        is_child,
        &format!("{}({}) -> {}", name, params_str, ret),
    )?;
    level += 2;
    pretty_write_stmt(f, level, true, &body)
}

fn pretty_write_decl(
    f: &mut Formatter<'_>,
    mut level: u32,
    is_child: bool,
    decl: &Declaration,
) -> fmt::Result {
    match decl {
        StatementDecl(stmt) => pretty_write_stmt(f, level, is_child, stmt),
        VarDecl(id, expr) => {
            write(f, level, is_child, "var")?;
            level += 2;
            write(f, level, true, id)?;
            pretty_write_expr(f, level, true, expr)
        }
        FnDecl(name, params, return_ty, body) => pretty_write_callable(
            f,
            level,
            is_child,
            name,
            None,
            params,
            return_ty.clone(),
            body,
        ),
        MethodDecl {
            name,
            self_,
            params,
            return_ty,
            body,
        } => pretty_write_callable(
            f,
            level,
            is_child,
            name,
            self_.as_ref(),
            params,
            return_ty.clone(),
            body,
        ),
        ImplDecl {
            struct_name,
            methods,
        } => {
            write(
                f,
                level,
                is_child,
                &format!("impl {}", struct_name.get_id()),
            )?;
            level += 2;
            for fn_decl in methods {
                pretty_write_decl(f, level, true, fn_decl)?;
            }
            Ok(())
        }
        StructDecl(name, fields) => {
            write(f, level, is_child, &format!("struct {}", name.get_id()))?;
            level += 2;
            for (field, ty) in fields {
                write(
                    f,
                    level,
                    true,
                    &format!("{}: {}", field.get_id(), ty.get_id()),
                )?;
            }
            Ok(())
        }
    }
}

fn pretty_write_stmt(
    f: &mut Formatter<'_>,
    mut level: u32,
    is_child: bool,
    stmt: &Statement,
) -> fmt::Result {
    match stmt {
        ExpressionStmt(expr) => pretty_write_expr(f, level, is_child, expr),
        If(cond, then_stmt, else_stmt) => {
            write(f, level, is_child, "if")?;
            level += 2;
            pretty_write_expr(f, level, true, cond)?;
            pretty_write_stmt(f, level, true, then_stmt)?;
            if let Some(else_) = else_stmt {
                pretty_write_stmt(f, level, true, else_)?;
            }
            Ok(())
        }
        While(cond, body) => {
            write(f, level, is_child, "while")?;
            level += 2;
            pretty_write_expr(f, level, true, cond)?;
            pretty_write_stmt(f, level, true, body)
        }
        Print(expr) => {
            write(f, level, is_child, "print")?;
            level += 2;
            pretty_write_expr(f, level, true, expr)
        }
        Ret(expr) => {
            write(f, level, is_child, "return")?;
            level += 2;
            match expr {
                Some(expr_) => pretty_write_expr(f, level, true, expr_),
                None => write(f, level, true, "None"),
            }
        }
        Block(decls) => {
            write(f, level, is_child, "Block")?;
            level += 2;
            for decl in decls.iter() {
                pretty_write_decl(f, level, true, decl)?;
            }
            Ok(())
        }
    }
}

pub fn file_to_string<P: AsRef<Path>>(path: &P) -> Result<String, Box<dyn std::error::Error>> {
    let full_path = env::current_dir().unwrap().join(path);
    let mut file = File::open(full_path)?;
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf)?;

    let res = String::from_utf8(file_buf)?;
    Ok(res)
}

pub fn run(program: String, options: &Options) {
    let mut lexer = crate::lexer::Lexer::new();
    lexer.lex(&program);
    if options.tokens {
        lexer.print_tokens();
    }

    if options.until == 1 {
        return;
    }
    let mut parser = crate::parser::Parser::new(&lexer);
    match parser.parse() {
        Ok(prog) => {
            if options.ast {
                crate::util::pretty_print(&prog);
            }

            if options.until == 2 {
                return;
            }
            let mut checker = crate::type_checker::TypeChecker::new();
            match checker.check(&prog, options.symbols) {
                Ok(()) => {
                    if options.until == 3 {
                        return;
                    }
                    let mut compiler: crate::compiler::Compiler = Default::default();
                    compiler.compile(&prog);

                    if options.chunk {
                        println!("{}", compiler.chunk());
                    }

                    if options.vm {
                        println!("\nVirtual Machine");
                        println!("===============");
                    }

                    if options.until == 4 {
                        return;
                    }

                    let mut vm = crate::vm::VM::new(options.vm);
                    vm.interpet(compiler.chunk());
                }
                Err(err) => {
                    print_error(&program, err.token_range, &err.message);
                }
            }
        }
        Err(parser_err) => {
            print_error(&program, parser_err.token_range, &parser_err.message);
        }
    }
}

fn print_error(prog: &str, range: std::ops::Range<usize>, msg: &str) {
    let mut newline_before_token = 0;
    // Initialized to prog len in case of last line
    let mut newline_after_token = prog.len();
    let mut line_count = 1;

    let mut break_on_next_newline = false;
    for (i, ch) in prog.bytes().enumerate() {
        if i >= range.start {
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

    let token_start_index = range.start - newline_before_token;
    let token_end_index = range.end - newline_before_token;
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
