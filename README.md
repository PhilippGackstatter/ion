# ION

A type-checked, interpreted programming language.

## Example

A classic fibonacci example.

Run this using `cargo run --bin file -- examples/fibonacci.io`

```
// Prints the fibonacci numbers until `up_to`
// and returns the last one that was calculated
fn fibonacci(up_to: i32) -> i32 {

    var fib1 = 0;
    var fib2 = 1;

    var next = 0;

    while(next <= up_to) {

        print next;

        fib1 = fib2;
        fib2 = next;
        next = fib1 + fib2;

    }

    return next;

}

// The type of `last` can be inferred from the function signature
var last = fibonacci(100);
print last;
```

This program would print `0 1 1 2 3 5 8 13 21 34 55 89 144` (separated by newlines).

Types of constants and of function return values can be inferred, which leads to variable declarations without explicit type annotations.

Since it is type checked at compile time, calling `fibonacci("100")` would error with

```
20: fibonacci("100");
              ^^^^^ Function parameters have incompatible type. Expected: i32, Supplied: str.
```

Another Example with Structs.

```
struct Dog {
    name: str,
    age: i32,
}

struct Owner {
    name: str,
    dog: Dog,
}

fn execute() {
    var owner = Owner {
        name: "John",
        dog: Dog {
            name: "Nemo",
            age: 2,
        },
    };

    owner.name = "John, the dog owner";

    print owner.name;
    print owner.dog.name;
}

execute();
```

That would print

```
John, the dog owner
Nemo
```

## Run

All you need is `cargo` & `rustc`. Written with `rustc 1.37.0`, but should work with much earlier versions as well.

Pass the `--help` option to see the options.

```sh
cargo run --bin file -- --help

Usage:
  target/debug/file [OPTIONS] [FILE]

ion language

Positional arguments:
  file                  The ion file to execute.

Optional arguments:
  -h,--help             Show this help message and exit
  -t,--tokens           Print the tokens produced by the Lexer.
  -a,--ast              Print the abstract syntax tree produced by the Parser.
  -s,--symbols          Print the symbol table produced by the Type Checker.
  -c,--chunk            Print the Chunk (Constants + Bytecode) produced by the
                        Compiler.
  -v,--vm               Print the execution trace by the VM.
  -u,--until UNTIL      Until what stage to run: 1 Lexer, 2 Parser, 3 Type
                        Checker, 4 Compiler, 5 Virtual Machine.
```

Run sample scripts

```sh
cargo run --bin file -- examples/function.io
```

or run an interactive REPL

```sh
cargo run --bin repl
```

## Architecture

The basic architecture is inspired by the lox language from [craftinginterpreters.com](https://craftinginterpreters.com). The ion compiler is multi-pass, as opposed to `clox` from the book. This implementation takes some parts, like the parser, from `jlox` and some, like the bytecode compiler and virtual machine, from `clox`.
Let's walk through the architecture, by using some sample input.

```
var x = 1;

if (x < 2)
    x = 1 + x * 3;

print x;
```

The compiler first scans the input, which produces tokens.

```
VarToken
IdToken("x")
Equal
Num(1)
Semicolon
IfToken
LeftParen
IdToken("x")
Less
Num(2)
RightParen
IdToken("x")
Equal
Num(1)
Plus
IdToken("x")
Star
Num(3)
Semicolon
PrintToken
IdToken("x")
Semicolon
EndOfFile
```

The recursive-descent parser then produces an abstract syntax tree to represent the structure of the program including precedence. You can see this most clearly in the `1 + x * 3` expression, which correctly nests the multiplication inside the add operation, meaning it is executed first.
Note that the abstract syntax tree is on the top level, a list of declarations (or statements in this case), here a variable, if and print statement.
Each of these is then represented as a tree.

```
─ var
  └─ x
  └─ 1
─ if
  └─ Less
    └─ 2
    └─ x
  └─ Assign
    └─ x
    └─ Plus
      └─ Star
        └─ 3
        └─ x
      └─ 1
─ print
  └─ x
```

The compiler takes the AST, walks it in post-order and produces a Chunk. A chunk contains constants like the `1` from the variable declaration.
It also contains bytecode. In this human-friendly representation, you can see the OpCode names and values being loaded or indexes being jumped to.

```
0 OpConstant 1
3 OpDefineGlobal x
6 OpGetGlobal x
9 OpConstant 2
12 OpLess
13 OpJumpIfFalse -> 30
16 OpConstant 1
19 OpGetGlobal x
22 OpConstant 3
25 OpMul
26 OpAdd
27 OpSetGlobal x
30 OpGetGlobal x
33 OpPrint
34 OpReturn
```

Next we can give that chunk to the virtual machine and execute it. The VM is stack-based.
In this representation, the first line shows the stack contents, the next the OpCode being executed, the next the stack contents after the opcode, and so on.

```
[ ]
OpConstant
[1]
OpDefineGlobal
[ ]
OpGetGlobal
[1]
OpConstant
[1] [2]
OpLess
[true]
OpJumpIfFalse
[ ]
OpConstant
[1]
OpGetGlobal
[1] [1]
OpConstant
[1] [1] [3]
OpMul
[1] [3]
OpAdd
[4]
OpSetGlobal
[ ]
OpGetGlobal
[4]
OpPrint
4
[ ]
OpReturn
```

Run a sample script or write your own to see all of this being created for your program.

## Errors

Parser Errors are nicely printed out.

For the input

```
var myvar = 0;

if myvar > 2)
    print myvar;
```

it prints

```
3: if myvar > 2)
      ^^^^^ Expected '(' after if.
```

The type checker also prints nice messages.

```
fn foo(first: str, second: i32, third: bool, fourth: bool) {
    print first;
}

foo("str", 12, true, 25);
```

Attempting to run this produces

```
6: bar("str", 12, true, 25);
                        ^^ Function parameters have incompatible type. Expected: bool, Supplied: i32.
```
