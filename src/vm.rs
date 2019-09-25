use crate::types::{
    Bytecode::{self, *},
    Chunk, Object, Token,
    TokenKind::{self, *},
    Value,
};
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::with_capacity(16),
            globals: HashMap::new(),
        }
    }

    pub fn interpet(&mut self, chunk: &Chunk) {
        let code = &chunk.code;
        let mut i = 0;
        println!("\nVirtual Machine");
        println!("===============");

        while i < code.len() {
            self.debug_stack();
            let byte = unsafe { std::mem::transmute::<u8, Bytecode>(code[i]) };
            println!("{:?}", byte);
            match byte {
                OpConstant => {
                    let index = self.read_u16(chunk, &mut i);
                    self.push(chunk.constants[index as usize].clone());
                }
                OpDefineGlobal => {
                    let index = self.read_u16(chunk, &mut i);
                    let assign = self.pop();
                    if let Value::Obj(Object::StringObj(str_)) = &chunk.constants[index as usize] {
                        self.globals.insert(str_.clone(), assign);
                    }
                }
                OpSetGlobal => {
                    let index = self.read_u16(chunk, &mut i);
                    let new_val = self.pop();
                    if let Value::Obj(Object::StringObj(str_)) = &chunk.constants[index as usize] {
                        self.globals.insert(str_.clone(), new_val);
                    }
                }
                OpGetGlobal => {
                    let index = self.read_u16(chunk, &mut i);
                    if let Value::Obj(Object::StringObj(str_)) = &chunk.constants[index as usize] {
                        self.push(self.globals.get(str_).unwrap().clone());
                    }
                }
                OpJumpIfFalse => {
                    let index = self.read_u16(chunk, &mut i);
                    if self.pop() == Value::Bool(false) {
                        i = index as usize - 1;
                    }
                }
                OpJump => {
                    let index = self.read_u16(chunk, &mut i);
                    i = index as usize - 1;
                }
                OpLoop => {
                    let index = self.read_u16(chunk, &mut i);
                    i = index as usize - 1;
                }
                OpPrint => {
                    let arg = self.pop();
                    println!("{}", arg);
                }
                OpAdd | OpMul | OpSub | OpDiv | OpGreater | OpLess => {
                    self.apply_binary_op(byte);
                }
                OpReturn => {}
                _ => panic!("Unkown opcode {}", byte as u8),
            }

            i += 1;
        }
        self.debug_stack();
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .unwrap_or_else(|| panic!("Nothing on the stack."))
    }

    fn apply_binary_op(&mut self, byte: Bytecode) {
        let second = self.pop();
        let first = self.pop();
        if let Value::Int(i1) = first {
            if let Value::Int(i2) = second {
                match byte {
                    OpAdd => {
                        self.push(Value::Int(i1 + i2));
                    }
                    OpMul => {
                        self.push(Value::Int(i1 * i2));
                    }
                    OpSub => {
                        self.push(Value::Int(i1 - i2));
                    }
                    OpDiv => {
                        self.push(Value::Int(i1 / i2));
                    }
                    OpGreater => {
                        self.push(Value::Bool(i1 > i2));
                    }
                    OpLess => {
                        self.push(Value::Bool(i1 < i2));
                    }
                    _ => unimplemented!(),
                }
            } else {
                panic!("Operands must be of same type");
            }
        } else if let Value::Bool(b1) = first {
            if let Value::Bool(b2) = second {
                match byte {
                    _ => unimplemented!(),
                }
            } else {
                panic!("Operands must be of same type");
            }
        } else {
            unimplemented!();
        }
    }

    fn read_u16(&self, chunk: &Chunk, i: &mut usize) -> u16 {
        *i += 1;
        let b1 = chunk.code[*i];
        *i += 1;
        let b2 = chunk.code[*i];
        u16::from(b1) << 8 | u16::from(b2)
    }

    // Debug Helper

    fn debug_stack(&self) {
        if !self.stack.is_empty() {
            for elem in self.stack.iter() {
                print!("[{}] ", elem);
            }
        } else {
            print!("[ ]");
        }
        println!();
    }
}
