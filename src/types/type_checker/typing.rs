use std::{
    cell::RefCell,
    collections::HashMap,
    ops::Range,
    rc::{Rc, Weak},
};

use crate::types::{Function, Struct, Trait};

pub type RcType = Rc<RefCell<Type>>;
pub type WeakType = Weak<RefCell<Type>>;

#[derive(Debug, Clone)]
pub struct LocatedType {
    pub token_range: Range<usize>,
    pub typ: RcType,
}

impl LocatedType {
    pub fn new(token_range: Range<usize>, kind: RcType) -> Self {
        LocatedType {
            token_range,
            typ: kind,
        }
    }

    pub(crate) fn new_empty_range(kind: RcType) -> Self {
        LocatedType {
            token_range: 0..0,
            typ: kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    /// The traits that this struct implements.
    pub traits: HashMap<String, RcType>,
    pub kind: TypeKind,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self {
            kind,
            traits: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Integer,
    Double,
    Str,
    Bool,
    Void,
    Func(Function),
    Struct(Struct),
    Trait(Trait),
}

impl PartialEq for LocatedType {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Ignore traits in display for now.
        self.kind.fmt(f)
    }
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Integer => write!(f, "i32"),
            TypeKind::Double => write!(f, "f32"),
            TypeKind::Str => write!(f, "str"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Trait(trt) => {
                write!(f, "trait {}", trt.name)?;
                if !trt.methods.is_empty() {
                    write!(f, " (")?;
                    for method in trt.methods.iter() {
                        write!(f, "{}", fmt_typekind_exit(method.1.clone()))?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            TypeKind::Struct(strct) => {
                write!(f, "struct {}", strct.name)?;
                if !strct.fields.is_empty() {
                    write!(f, " (")?;

                    let stringified = strct
                        .fields
                        .iter()
                        .map(|field| format!("{}: {}", field.0, fmt_typekind_exit(field.1.clone())))
                        .collect::<Vec<String>>()
                        .join(", ");

                    write!(f, "{stringified})")?;
                }
                Ok(())
            }
            TypeKind::Func(function) => {
                write!(f, "{}(", function.name)?;

                let stringified = function
                    .params
                    .iter()
                    .map(|param| fmt_typekind_exit(param.clone()).to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}", stringified)?;

                write!(
                    f,
                    ") -> {}",
                    if let Some(ret_ty) = &function.result {
                        fmt_typekind_exit(ret_ty.clone())
                    } else {
                        "void".into()
                    }
                )
            }
        }
    }
}

fn fmt_typekind_exit(ty: WeakType) -> String {
    let ty = ty.upgrade().unwrap();
    let ty_ref = ty.borrow();
    match &ty_ref.kind {
        TypeKind::Func(func) => func.name.clone(),
        TypeKind::Struct(struct_) => struct_.name.clone(),
        other => format!("{}", other),
    }
}

impl std::fmt::Display for LocatedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", *self.typ.borrow())
    }
}
