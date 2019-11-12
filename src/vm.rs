use crate::types::{
    Bytecode::{self, *},
    Chunk, Object, Value,
};
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct VM {
    print_debug: bool,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    frame_pointer: usize,
}

impl VM {
    pub fn new(print_debug: bool) -> Self {
        VM {
            print_debug,
            stack: Vec::with_capacity(16),
            globals: HashMap::new(),
            // An index into the stack where the frame for the
            // currently executing function starts
            frame_pointer: 0,
        }
    }

    pub fn interpet(&mut self, chunk: &Chunk) {
        let code = &chunk.code;
        let mut i = 0;

        loop {
            if self.print_debug {
                self.debug_stack();
            }

            let byte = unsafe { std::mem::transmute::<u8, Bytecode>(code[i]) };

            if self.print_debug {
                eprintln!("{:?}", byte);
            }

            match byte {
                OpPop => {
                    self.pop();
                }
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
                OpGetLocal => {
                    let index = self.read_u8(chunk, &mut i);
                    self.push(self.stack[self.frame_pointer + index as usize].clone());
                }
                OpSetLocal => {
                    let index = self.read_u8(chunk, &mut i) as usize;
                    self.stack[self.frame_pointer + index] = self.pop();
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
                OpStructInit => {
                    let struct_length = self.read_u8(chunk, &mut i);
                    let mut field_value_map = HashMap::new();
                    for _ in 0..struct_length {
                        let field_name = self.pop();
                        let field_value = self.pop();

                        if let Value::Obj(Object::StringObj(field_name)) = field_name {
                            field_value_map.insert(field_name, field_value);
                        } else {
                            panic!("Expected string as field name.");
                        }
                    }
                    self.push(Value::Obj(Object::StructObj {
                        fields: field_value_map,
                    }));
                }
                OpStructAccess => {
                    let field_name = self.pop();
                    let struct_ = self.pop();
                    if let Value::Obj(Object::StringObj(field_name)) = field_name {
                        if let Value::Obj(Object::StructObj { fields }) = struct_ {
                            self.push(fields.get(&field_name).unwrap().clone());
                        } else {
                            panic!("Expected struct");
                        }
                    } else {
                        panic!("Expected string as field name");
                    }
                }
                OpCall => {
                    if let Value::Obj(Object::FnObj(_name, chunk, arity)) = self.pop() {
                        // Save the position of the current frame pointer
                        let current_frame_pointer = self.frame_pointer;

                        // The frame pointer points to the first parameter of the function on the stack
                        self.frame_pointer = self.stack.len() - arity as usize;

                        self.interpet(&chunk);

                        // And restore it after the function returns
                        self.frame_pointer = current_frame_pointer;
                    } else {
                        panic!("Expected function object on the stack.");
                    }
                }
                OpPrint => {
                    let arg = self.pop();
                    println!("{}", arg);
                }
                OpAdd | OpMul | OpSub | OpDiv | OpGreater | OpLess | OpGreaterEqual
                | OpLessEqual => {
                    self.apply_number_op(byte);
                }
                OpNot | OpEqual => {
                    self.apply_bool_op(byte);
                }
                OpReturn => {
                    let return_value = if self.read_u8(&chunk, &mut i) == 1 {
                        Some(self.pop())
                    } else {
                        None
                    };

                    let num_locals = self.read_u8(&chunk, &mut i);
                    for _ in 0..num_locals {
                        self.pop();
                    }

                    if let Some(ret_val) = return_value {
                        self.push(ret_val);
                    }
                    break;
                }
                _ => panic!("Unkown opcode {}", byte as u8),
            }

            i += 1;
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .unwrap_or_else(|| panic!("Nothing on the stack."))
    }

    fn apply_number_op(&mut self, byte: Bytecode) {
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
                    OpGreaterEqual => {
                        self.push(Value::Bool(i1 >= i2));
                    }
                    OpLess => {
                        self.push(Value::Bool(i1 < i2));
                    }
                    OpLessEqual => {
                        self.push(Value::Bool(i1 <= i2));
                    }
                    _ => unimplemented!(),
                }
            } else {
                panic!("Operands must be of same type");
            }
        } else {
            unimplemented!();
        }
    }

    fn apply_bool_op(&mut self, byte: Bytecode) {
        match byte {
            OpEqual => {
                let res = self.pop().unwrap_bool() == self.pop().unwrap_bool();
                self.push(Value::Bool(res));
            }
            OpNot => {
                let res = !self.pop().unwrap_bool();
                self.push(Value::Bool(res));
            }
            _ => unimplemented!(),
        }
    }

    fn read_u8(&self, chunk: &Chunk, i: &mut usize) -> u8 {
        *i += 1;
        chunk.code[*i]
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
                eprint!("[{}] ", elem);
            }
        } else {
            eprint!("[ ]");
        }
        eprintln!();
    }
}
