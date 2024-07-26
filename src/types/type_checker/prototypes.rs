use std::collections::{HashMap, HashSet};

use arenas::EntryId;

use crate::types::{IdentifierToken, MethodHeader, Statement};

pub type PrototypeId = EntryId;

#[derive(Debug, Clone, PartialEq)]
pub struct Prototype {
    /// The traits that this struct implements.
    pub traits: HashSet<IdentifierToken>,
    /// The methods this type has.
    /// For traits, these are the trait methods.
    pub methods: HashMap<String, MethodHeader>,
    pub kind: PrototypeKind,
}

impl Prototype {
    pub fn new(kind: PrototypeKind) -> Self {
        Self {
            traits: HashSet::new(),
            methods: HashMap::new(),
            kind,
        }
    }

    pub fn with_methods(mut self, methods: HashMap<String, MethodHeader>) -> Self {
        self.methods = methods;
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrototypeKind {
    Integer,
    Double,
    Str,
    Bool,
    Void,
    Struct(ProtoStruct),
    Function(ProtoFunction),
    Method(ProtoMethod),
    Trait(ProtoTrait),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtoStruct {
    pub name: IdentifierToken,
    pub fields: Vec<(IdentifierToken, IdentifierToken)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtoFunction {
    pub name: IdentifierToken,
    pub params: Vec<IdentifierToken>,
    pub result: Option<IdentifierToken>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtoTrait {
    pub name: IdentifierToken,
    pub method_bodies: HashMap<String, Statement>,
}

pub type ProtoMethod = MethodHeader;

impl std::fmt::Display for Prototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            PrototypeKind::Integer => write!(f, "i32"),
            PrototypeKind::Double => write!(f, "f32"),
            PrototypeKind::Str => write!(f, "str"),
            PrototypeKind::Bool => write!(f, "bool"),
            PrototypeKind::Void => write!(f, "void"),
            PrototypeKind::Trait(trt) => {
                write!(f, "trait {}", trt.name)
            }
            PrototypeKind::Struct(strct) => {
                write!(f, "struct {}", strct.name)
            }
            PrototypeKind::Function(function) => {
                write!(f, "{}(", function.name)?;

                let stringified = function
                    .params
                    .iter()
                    .map(|param| param.as_str().to_owned())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}", stringified)?;

                write!(
                    f,
                    ") -> {}",
                    if let Some(ret_ty) = &function.result {
                        ret_ty.name.to_owned()
                    } else {
                        "void".into()
                    }
                )
            }
            PrototypeKind::Method(method) => {
                // TODO: Align with function.
                write!(f, "{}", method.name)
            }
        }
    }
}
