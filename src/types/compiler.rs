use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

use crate::types::Declaration;

pub type Program = Vec<Declaration>;

#[derive(Debug)]
#[repr(u8)]
pub enum Bytecode {
    OpPop,
    OpMul,
    OpAdd,
    OpDiv,
    OpSub,
    OpNot,
    OpEqual,
    OpNegate,
    OpGreater,
    OpLess,
    OpGreaterEqual,
    OpLessEqual,
    OpConstant,
    OpDefineGlobal,
    OpSetGlobal,
    OpGetGlobal,
    OpSetLocal,
    OpGetLocal,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
    OpCall,
    OpStructInit,
    OpStructGetField,
    OpStructSetField,
    OpStructMethod,
    OpPrint,
    OpReturn,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            constants: vec![],
            code: vec![],
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Int(i32),
    Double(f32),
    Obj(Rc<RefCell<Object>>),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Value::Bool(boolean) => write!(f, "{:?}", boolean),
            Value::Int(int) => write!(f, "{:?}", int),
            Value::Double(double) => write!(f, "{:?}", double),
            Value::Obj(obj) => write!(f, "{:?}", obj.borrow().clone()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(lboolean), Value::Bool(rboolean)) => lboolean == rboolean,
            (Value::Int(lint), Value::Int(rint)) => lint == rint,
            (Value::Double(ldouble), Value::Double(rdouble)) => ldouble == rdouble,
            (Value::Obj(lobj), Value::Obj(robj)) => lobj.borrow().clone() == robj.borrow().clone(),
            (_, _) => false,
        }
    }
}

impl Value {
    pub fn unwrap_bool(self) -> bool {
        if let Value::Bool(b) = self {
            b
        } else {
            panic!("Expected bool.");
        }
    }

    pub fn unwrap_obj(self) -> Rc<RefCell<Object>> {
        if let Value::Obj(obj) = self {
            obj
        } else {
            panic!("Expected Object.");
        }
    }

    pub fn unwrap_int(self) -> i32 {
        if let Value::Int(int) = self {
            int
        } else {
            panic!("Expected i32.");
        }
    }

    pub fn unwrap_double(self) -> f32 {
        if let Value::Double(double) = self {
            double
        } else {
            panic!("Expected f32.");
        }
    }

    pub fn unwrap_string(self) -> String {
        if let Object::StringObj(str_) = self.unwrap_obj().borrow().clone() {
            str_
        } else {
            panic!("Expected string");
        }
    }

    pub fn unwrap_struct(self) -> HashMap<String, Value> {
        let obj = self.unwrap_obj().borrow().clone();
        if let Object::StructObj { fields, .. } = obj {
            fields
        } else {
            panic!("Expected struct, got {:?}", obj);
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    StringObj(String),
    FnObj {
        name: String,
        receiver: Option<Rc<RefCell<Object>>>,
        chunk: Chunk,
        arity: u8,
    },
    StructObj {
        name: String,
        fields: HashMap<String, Value>,
    },
}

impl Object {
    pub fn unwrap_string(self) -> String {
        if let Object::StringObj(str_) = self {
            str_
        } else {
            panic!("Expected String.");
        }
    }

    pub fn unwrap_struct(self) -> HashMap<String, Value> {
        if let Object::StructObj { fields, .. } = self {
            fields
        } else {
            panic!("Expected struct obj.");
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(int) => write!(f, "{}", int),
            Value::Double(float) => write!(f, "{}", float),
            Value::Obj(obj) => write!(f, "{}", obj.borrow().clone()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::StringObj(str_) => write!(f, "{}", str_),
            Object::FnObj {
                name,
                arity,
                receiver,
                ..
            } => {
                if let Some(recv) = receiver {
                    write!(f, "{} ({}) <{}>", name, arity, recv.borrow())
                } else {
                    write!(f, "{} ({})", name, arity)
                }
            }
            Object::StructObj { name, .. } => write!(f, "{} {{}}", name),
        }
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\nChunk")?;
        writeln!(f, "=====")?;
        writeln!(f, "\nConstants")?;
        for (i, con) in self.constants.iter().enumerate() {
            writeln!(f, "{} -> {}", i, con)?;
        }

        writeln!(f, "\nBytecode")?;
        let mut iter = self.code.iter().enumerate();
        while let Some((i, str_)) = byte_to_opcode(&mut iter, &self.constants) {
            writeln!(f, "{} {}", i, str_)?;
        }
        Ok(())
    }
}

fn byte_to_opcode(
    bytes: &mut std::iter::Enumerate<std::slice::Iter<u8>>,
    constants: &[Value],
) -> Option<(usize, String)> {
    use Bytecode::*;

    if let Some((index, byte_)) = bytes.next() {
        let byte = unsafe { std::mem::transmute::<u8, Bytecode>(*byte_) };
        let res = match byte {
            OpPop | OpMul | OpAdd | OpDiv | OpSub | OpNot | OpEqual | OpNegate | OpGreater
            | OpLess | OpGreaterEqual | OpLessEqual | OpPrint | OpStructGetField | OpCall
            | OpStructSetField => format!("{:?}", byte),
            OpConstant => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpDefineGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpSetGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpGetGlobal => {
                let index = read_u16(bytes);
                format!("{:?} {}", byte, constants[index as usize])
            }
            OpSetLocal => {
                let index = read_u8(bytes);
                format!("{:?} {}", byte, index)
            }
            OpGetLocal => {
                let index = read_u8(bytes);
                format!("{:?} {}", byte, index)
            }
            OpJumpIfFalse => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            OpJump => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            OpLoop => {
                let index = read_u16(bytes);
                format!("{:?} -> {}", byte, index)
            }
            OpStructInit => {
                let index = read_u8(bytes);
                format!("{:?} of len {}", byte, index)
            }
            OpStructMethod => {
                let index = read_u16(bytes);
                let struct_name = &constants[index as usize];
                let index = read_u16(bytes);
                let method_name = &constants[index as usize];
                format!("{:?} Add {} to {}", byte, method_name, struct_name)
            }
            OpReturn => {
                let retvals = read_u8(bytes);
                let pop = read_u8(bytes);
                format!("{:?} {} vals, pop {}", byte, retvals, pop)
            }
        };
        Some((index, res))
    } else {
        None
    }
}

fn read_u8(bytes: &mut std::iter::Enumerate<std::slice::Iter<u8>>) -> u8 {
    let (_, b1) = bytes.next().unwrap();
    *b1
}

fn read_u16(bytes: &mut std::iter::Enumerate<std::slice::Iter<u8>>) -> u16 {
    let (_, b1) = bytes.next().unwrap();
    let (_, b2) = bytes.next().unwrap();
    u16::from(*b1) << 8 | u16::from(*b2)
}
