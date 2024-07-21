use argparse::{ArgumentParser, Store, StoreTrue};
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::types::{
    CompilationErrorContext, CompileError,
    Declaration::{self, *},
    Expression,
    ExpressionKind::*,
    IdentifierToken, MethodDeclaration, MethodSelf, Program,
    Statement::{self, *},
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
            write(
                f,
                level,
                is_child,
                &format!("Struct {}", name.unwrap_identifier()),
            )?;
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

#[allow(clippy::too_many_arguments)]
fn pretty_write_callable(
    f: &mut Formatter<'_>,
    mut level: u32,
    is_child: bool,
    name: &str,
    self_: Option<&MethodSelf>,
    params: &[(IdentifierToken, IdentifierToken)],
    return_ty: Option<IdentifierToken>,
    body: &Statement,
) -> fmt::Result {
    let ret = if let Some(identifier) = return_ty.as_ref() {
        identifier.name.clone()
    } else {
        "void".to_owned()
    };

    let mut params_str: String = {
        let mut other_params_separator = "";
        if !params.is_empty() {
            other_params_separator = ", ";
        }

        match self_ {
            Some(method_self) => match &method_self.type_token {
                Some(type_token) => {
                    format!("self: {}{other_params_separator}", type_token)
                }
                None => format!("self{other_params_separator}"),
            },
            None => "".to_owned(),
        }
    };

    params_str.push_str(
        &params
            .iter()
            .map(|(param_name, param_type)| format!("{param_name}: {param_type}"))
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
    pretty_write_stmt(f, level, true, body)
}

fn pretty_write_decl(
    f: &mut Formatter<'_>,
    mut level: u32,
    is_child: bool,
    decl: &Declaration,
) -> fmt::Result {
    match decl {
        FnDecl {
            identifier: name,
            params,
            return_ty,
            body,
        } => pretty_write_callable(
            f,
            level,
            is_child,
            name.as_str(),
            None,
            params,
            return_ty.clone(),
            body,
        ),
        TraitDecl {
            trait_identifier: trait_name,
            methods,
        } => {
            write(f, level, is_child, &format!("trait {}", trait_name))?;
            for method in methods {
                let MethodDeclaration {
                    name,
                    self_,
                    params,
                    return_ty,
                    body,
                } = method;

                pretty_write_callable(
                    f,
                    level,
                    true,
                    name.as_str(),
                    self_.as_ref(),
                    params,
                    return_ty.clone(),
                    body,
                )?;
            }
            Ok(())
        }
        ImplDecl {
            type_name: struct_name,
            trait_name,
            methods,
        } => {
            let stringified = match trait_name {
                Some(trait_name) => {
                    format!("impl {} for {}", trait_name.as_str(), struct_name.as_str())
                }
                None => format!("impl {}", struct_name),
            };

            write(f, level, is_child, &stringified)?;
            level += 2;
            for method in methods {
                let MethodDeclaration {
                    name,
                    self_,
                    params,
                    return_ty,
                    body,
                } = method;

                pretty_write_callable(
                    f,
                    level,
                    true,
                    name.as_str(),
                    self_.as_ref(),
                    params,
                    return_ty.clone(),
                    body,
                )?;
            }
            Ok(())
        }
        StructDecl { identifier, fields } => {
            write(f, level, is_child, &format!("struct {}", identifier))?;
            level += 2;
            for (field, ty) in fields {
                write(f, level, true, &format!("{}: {}", field, ty))?;
            }
            Ok(())
        }
        ImplMethodDecl {
            type_name,
            trait_name,
            method,
        } => {
            let stringified = match trait_name {
                Some(trait_name) => {
                    format!("impl {} for {}", trait_name.as_str(), type_name.as_str())
                }
                None => format!("impl {}", type_name),
            };

            write(f, level, is_child, &stringified)?;

            let MethodDeclaration {
                name,
                self_,
                params,
                return_ty,
                body,
            } = method;

            pretty_write_callable(
                f,
                level,
                true,
                name.as_str(),
                self_.as_ref(),
                params,
                return_ty.clone(),
                body,
            )?;

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
        Block(statements) => {
            write(f, level, is_child, "Block")?;
            level += 2;
            for statement in statements.iter() {
                pretty_write_stmt(f, level, true, statement)?;
            }
            Ok(())
        }
        LetBinding(id, expr) => {
            write(f, level, is_child, "let")?;
            level += 2;
            write(f, level, true, id)?;
            pretty_write_expr(f, level, true, expr)
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
                    print_error(&program, err);
                }
            }
        }
        Err(parser_err) => print_error(&program, parser_err),
    }
}

pub(crate) fn print_error(program: &str, err: CompileError) {
    let error = err.display(CompilationErrorContext::new(program));
    println!("{error}");
}

pub(crate) fn display_error(prog: &str, range: std::ops::Range<usize>, msg: &str) -> String {
    let mut newline_before_token_start = 0;
    let mut newline_before_token_end = 0;
    // Initialized to prog len in case of last line
    let mut newline_after_token_end = prog.len();
    // The number of the line where the first character of the token appears.
    let mut first_line_number = 1;
    // The number of lines between the first character of the token and the end of the token,
    // i.e. how many lines to print in total.
    let mut line_count = 1;

    for (i, ch) in prog
        .bytes()
        .enumerate()
        .rev()
        .skip(prog.len() - range.start)
    {
        if ch == b'\n' {
            newline_before_token_start = i;
            break;
        }
    }

    for (i, ch) in prog.bytes().enumerate().rev().skip(prog.len() - range.end) {
        if ch == b'\n' {
            newline_before_token_end = i;
            break;
        }
    }

    for (i, ch) in prog.bytes().enumerate().skip(range.end) {
        if ch == b'\n' {
            newline_after_token_end = i;
            break;
        }
    }

    for (i, ch) in prog.bytes().enumerate() {
        if ch == b'\n' {
            if i < range.start {
                first_line_number += 1;
            } else if i >= range.start && i <= range.end {
                line_count += 1;
            } else {
                break;
            }
        }
    }

    // Handle beginning of file
    if newline_before_token_start != 0 {
        newline_before_token_start += 1
    };

    let token = &prog[newline_before_token_start..newline_after_token_end];

    // Get the number of characters needed to display the longest line number in the message.
    let line_number_indent = ((first_line_number + line_count) as f32).log10() as usize + 1;

    let token_split = token.split('\n');
    let mut error = String::new();

    for (line_number, line) in token_split.clone().enumerate() {
        let mut line_number_str: String = format!("{}", first_line_number + line_number);
        let line_number_str_len = line_number_str.len();
        if line_number_str_len < line_number_indent {
            // Indent to match ident of largest line number string.
            for _ in 0..(line_number_indent - line_number_str_len) {
                line_number_str.push(' ');
            }
        }

        writeln!(&mut error, "{} | {}", line_number_str, line)
            .expect("writing to String should succeed");
    }

    // Differentiate between single and multi line errors.
    // With single line errors we want to be precise within the line.
    // With multi line error we point at the entire construct.
    let (token_start_index, token_end_index) = if line_count == 1 {
        (
            range.start - newline_before_token_start,
            range.end - newline_before_token_end,
        )
    } else {
        token_split.fold((usize::MAX, 0), |acc, elem| {
            (
                acc.0.min(
                    elem.chars()
                        .enumerate()
                        .find(|(_, ch)| !ch.is_whitespace())
                        .map(|(pos, _)| pos)
                        .unwrap_or(usize::MAX),
                ),
                acc.1.max(elem.len()),
            )
        })
    };

    // Indent the following arrows.
    // +3 to account for ` | `.
    for _ in 0..line_number_indent + 3 {
        write!(&mut error, " ").expect("writing to String should succeed");
    }

    for i in 0..token_end_index {
        if i >= token_start_index {
            write!(&mut error, "^").expect("writing to String should succeed");
        } else {
            write!(&mut error, " ").expect("writing to String should succeed");
        }
    }

    writeln!(&mut error, " {}", msg).expect("writing to String should succeed");

    error
}
