# ION

A type-checked, interpreted, clean programming language.

## Example

An object-oriented example with vectors.

Run this using `cargo run --bin file -- examples/struct_vector.io`

```
// Declaring a new type called Vector
struct Vector
    x: i32
    y: i32
    z: i32

// Add methods to the previously defined type
impl Vector

    dot_product(other: Vector) -> i32
        return self.x * other.x + self.y * other.y + self.z * other.z

var z_unit_vector = Vector {
    x: 0,
    y: 0,
    z: 1,
}

var y_unit_vector = Vector {
    x: 0,
    y: 1,
    z: 0,
}

if z_unit_vector.dot_product(y_unit_vector) == 0
    print "Vectors are orthogonal!"
else
    print "Vectors are not orthogonal!"
```

If we run this, it prints `Vectors are orthogonal!`.

## Run

All you need to run `ion` is Rust, by installing it from [rustup.rs](https://rustup.rs/).

Invoke `ion` with the `--help` option to see the options.

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
cargo run --bin file -- examples/vector.io
```

## Architecture

The basic architecture is inspired by the lox language from [craftinginterpreters.com](https://craftinginterpreters.com). The ion compiler is multi-pass, as opposed to `clox` from the book. This implementation takes some parts, like the parser, from `jlox` and some, like the bytecode compiler and virtual machine, from `clox`.

Let's walk through the architecture, by using some simple input.

```
var x = 4

if x < 5
    print "Less than 5"
else
    print "5 or more"
```

The compiler first scans the input, which produces tokens.

```
VarToken
IdToken("x")
Equal
Num(4)
NewLine
IfToken
IdToken("x")
Less
Num(5)
NewLine
WhiteSpace(4)
PrintToken
String_("Less than 5")
NewLine
ElseToken
NewLine
WhiteSpace(4)
PrintToken
String_("5 or more")
EndOfFile
```

The recursive-descent parser then produces an abstract syntax tree to represent the structure of the program including precedence. Note that the abstract syntax tree is on the top level, a list of declarations, here a variable declaration and an if statement. Each of these is then represented as a tree.

```
─ var
  └─ x
  └─ 4

─ if
  └─ Less
    └─ 5
    └─ x
  └─ Block
    └─ print
      └─ "Less than 5"
  └─ Block
    └─ print
      └─ "5 or more"
```

The compiler takes the AST, walks it in post-order and produces a chunk. A chunk contains constants like the `4` from the variable declaration.
It also contains bytecode. In this human-friendly representation, you can see the OpCode names and values being loaded or indexes being jumped to.

```
0 OpConstant 4
3 OpDefineGlobal x
6 OpGetGlobal x
9 OpConstant 5
12 OpLess
13 OpJumpIfFalse -> 23
16 OpConstant Less than 5
19 OpPrint
20 OpJump -> 27
23 OpConstant 5 or more
26 OpPrint
27 OpReturn 0 vals, pop 0
```

Next we can give that chunk to the stack-based virtual machine and execute it.
In this representation, the first line shows the initial stack contents, the second line the OpCode being executed and the third line the stack contents after the opcode, and so on.

```
[ ]
OpConstant
[4]
OpDefineGlobal
[ ]
OpGetGlobal
[4]
OpConstant
[4] [5]
OpLess
[true]
OpJumpIfFalse
[ ]
OpConstant
[Less than 5]
OpPrint
Less than 5
[ ]
OpJump
[ ]
OpReturn
```

Run a sample script or write your own to see all of this being created for your program.
