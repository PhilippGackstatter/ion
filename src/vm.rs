use crate::types::{
    Bytecode::{self, *},
    Chunk, Value,
};

#[derive(Debug, Default)]
pub struct VM {
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::with_capacity(16),
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
                    i += 1;
                    let b1 = code[i];
                    i += 1;
                    let b2 = code[i];
                    let index = u16::from(b1) << 8 | u16::from(b2);
                    self.push(chunk.constants[index as usize]);
                }
                OpAdd => {
                    let second = self.pop();
                    let first = self.pop();
                    self.push(first + second);
                }
                OpMul => {
                    let second = self.pop();
                    let first = self.pop();
                    self.push(first * second);
                }
                OpSub => {
                    let second = self.pop();
                    let first = self.pop();
                    self.push(first - second);
                }
                OpDiv => {
                    let second = self.pop();
                    let first = self.pop();
                    self.push(first / second);
                }
                _ => (),
            }

            i += 1;
        }
        self.debug_stack();
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    // Debug Helper

    fn debug_stack(&self) {
        for elem in self.stack.iter() {
            print!("[{}] ", elem);
        }
        println!();
    }
}
