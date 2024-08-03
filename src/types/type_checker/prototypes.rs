use std::collections::HashMap;

use arenas::EntryId;

use crate::types::{IdentifierToken, MethodHeader, MethodSelf, Statement};

pub type PrototypeId = EntryId;

#[derive(Debug, Clone, PartialEq)]
pub struct Prototype {
    /// The traits that this struct implements.
    /// The value signals whether the trait implementation for this type has been checked,
    /// which is to avoid checking multiple times.
    pub traits: HashMap<IdentifierToken, TraitImplCheck>,
    /// The methods this type has.
    /// For traits, these are the trait methods.
    pub methods: HashMap<String, MethodHeader>,
    pub kind: PrototypeKind,
}

impl Prototype {
    pub fn new(kind: PrototypeKind) -> Self {
        Self {
            traits: HashMap::new(),
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

#[derive(Debug, Clone, PartialEq)]
pub enum TraitImplCheck {
    Checked,
    Unchecked,
}

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
                fmt_function(f, &function.name, &None, &function.params, &function.result)
            }
            PrototypeKind::Method(method) => {
                let params = method
                    .parameters
                    .iter()
                    .map(|(_, param_type)| param_type)
                    .cloned()
                    .collect();
                fmt_function(
                    f,
                    &method.name,
                    &method.method_self,
                    &params,
                    &method.return_type,
                )
            }
        }
    }
}

fn fmt_function(
    f: &mut std::fmt::Formatter<'_>,
    name: &IdentifierToken,
    method_self: &Option<MethodSelf>,
    params: &Vec<IdentifierToken>,
    result: &Option<IdentifierToken>,
) -> std::fmt::Result {
    write!(f, "{}(", name)?;

    let mut parameters = if method_self.is_some() {
        vec!["self".to_owned()]
    } else {
        Vec::new()
    };

    parameters.extend(params.iter().map(|param| param.as_str().to_owned()));
    let stringified = parameters.join(", ");
    write!(f, "{}", stringified)?;

    write!(
        f,
        ") -> {}",
        if let Some(ret_ty) = &result {
            ret_ty.name.to_owned()
        } else {
            "void".into()
        }
    )
}
