use crate::types::{
    Bytecode::{self, *},
    Chunk, Object, Value,
};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

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
                    let new_val = self.pop();
                    if let Value::Obj(obj) = &chunk.constants[index as usize] {
                        if let Object::StringObj(str_) = obj.borrow().clone() {
                            self.globals.insert(str_, new_val);
                        }
                    }
                }
                OpSetGlobal => {
                    let index = self.read_u16(chunk, &mut i);
                    let new_val = self.pop();
                    if let Value::Obj(obj) = &chunk.constants[index as usize] {
                        if let Object::StringObj(str_) = obj.borrow().clone() {
                            self.globals.insert(str_, new_val);
                        }
                    }
                }
                OpGetGlobal => {
                    let index = self.read_u16(chunk, &mut i);
                    if let Value::Obj(obj) = &chunk.constants[index as usize] {
                        if let Object::StringObj(str_) = obj.borrow().clone() {
                            self.push(self.globals.get(&str_).unwrap().clone());
                        }
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
                    let struct_name = self.pop().unwrap_string();

                    // Use a copy of the struct prototype if it exists, otherwise use an empty hmap
                    let mut field_value_map = if let Some(entry) = self.globals.get(&struct_name) {
                        // If an entry exists it contains only functions.
                        // If we simply clone the entry, the Rc<RefCell> will be cloned
                        // but not the nested Function Objects.
                        // Later, when we set the receiver, it will overwrite whatever other receiver
                        // was in there before.
                        // Consequently, each function must be deep-cloned.
                        let mut fn_objs = HashMap::new();
                        if let Value::Obj(obj) = entry {
                            if let Object::StructObj { fields, .. } = &mut *obj.borrow_mut() {
                                for (key, val) in fields.iter() {
                                    if let Value::Obj(inner_obj) = val {
                                        let fn_obj = (*inner_obj.borrow_mut()).clone();
                                        fn_objs.insert(
                                            key.clone(),
                                            Value::Obj(Rc::new(RefCell::new(fn_obj))),
                                        );
                                    }
                                }
                            }
                        }
                        fn_objs
                    } else {
                        HashMap::new()
                    };

                    let method_names: Vec<String> =
                        field_value_map.keys().map(|k| k.to_owned()).collect();

                    for _ in 0..struct_length {
                        let field_name = self.pop().unwrap_string();
                        let field_value = self.pop();
                        field_value_map.insert(field_name, field_value);
                    }

                    let struct_obj = Rc::new(RefCell::new(Object::StructObj {
                        name: struct_name,
                        fields: field_value_map,
                    }));

                    for method in method_names {
                        if let Object::StructObj { fields, .. } = &mut *struct_obj.borrow_mut() {
                            fields.entry(method).and_modify(|e| {
                                if let Value::Obj(obj) = e {
                                    if let Object::FnObj { receiver, .. } = &mut *obj.borrow_mut() {
                                        *receiver = Some(Rc::clone(&struct_obj));
                                    }
                                }
                            });
                        }
                    }

                    self.push(Value::Obj(struct_obj));
                }
                OpStructGetField => {
                    let field_name = self.pop().unwrap_string();
                    let fields = self.pop().unwrap_struct();
                    self.push(fields.get(&field_name).unwrap().clone());
                }
                OpStructSetField => {
                    let field_name = self.pop().unwrap_string();
                    let struct_ = self.pop();
                    let new_value = self.pop();

                    if let Value::Obj(obj) = struct_ {
                        if let Object::StructObj { fields, .. } = &mut *obj.borrow_mut() {
                            *fields.get_mut(&field_name).unwrap() = new_value;
                        }
                    }
                }
                OpStructMethod => {
                    let struct_name_index = self.read_u16(chunk, &mut i);
                    let struct_name = chunk.constants[struct_name_index as usize]
                        .clone()
                        .unwrap_string();

                    let method_name_index = self.read_u16(chunk, &mut i);
                    let method_name = &chunk.constants[method_name_index as usize];

                    let method = self.pop();

                    let struct_entry = self.globals.entry(struct_name.clone());

                    match struct_entry {
                        Entry::Occupied(mut e) => {
                            if let Value::Obj(obj) = e.get_mut() {
                                if let Object::StructObj { fields, .. } = &mut *obj.borrow_mut() {
                                    fields.insert(method_name.clone().unwrap_string(), method);
                                } else {
                                    panic!("Should not call OpStructMethod on a non-struct.")
                                }
                            } else {
                                panic!("Should not call OpStructMethod on a non-object.")
                            }
                        }
                        Entry::Vacant(e) => {
                            let mut fields = HashMap::new();
                            fields.insert(method_name.clone().unwrap_string(), method);

                            let struct_obj = Rc::new(RefCell::new(Object::StructObj {
                                name: struct_name,
                                fields,
                            }));
                            e.insert(Value::Obj(struct_obj));
                        }
                    }
                }
                OpCall => {
                    if let Value::Obj(ref mut obj) = self.pop() {
                        if let Object::FnObj {
                            chunk,
                            arity,
                            receiver,
                            ..
                        } = &mut *obj.borrow_mut()
                        {
                            // Save the position of the current frame pointer
                            let current_frame_pointer = self.frame_pointer;

                            // The frame pointer points to the first parameter of the function on the stack
                            self.frame_pointer = self.stack.len() - *arity as usize;
                            if let Some(recv) = receiver {
                                // If we have a method, then we have the additional argument self at local index 0
                                self.frame_pointer -= 1;
                                self.stack[self.frame_pointer] = Value::Obj(Rc::clone(recv));
                            }

                            self.interpet(chunk);

                            // And restore it after the function returns
                            self.frame_pointer = current_frame_pointer;
                        } else {
                            panic!("Expected a function object on the stack.");
                        }
                    } else {
                        panic!("Expected an object on the stack.");
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
                    let return_value = if self.read_u8(chunk, &mut i) == 1 {
                        Some(self.pop())
                    } else {
                        None
                    };

                    let num_locals = self.read_u8(chunk, &mut i);
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
                let res = self.pop() == self.pop();
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
