use std::{
    cell::RefCell,
    ops::Range,
    rc::{Rc, Weak},
};

use crate::types::{Function, Struct, Trait};

pub type RcTypeKind = Rc<RefCell<TypeKind>>;
pub type WeakTypeKind = Weak<RefCell<TypeKind>>;

#[derive(Debug, Clone)]
pub struct Type {
    pub token_range: Range<usize>,
    pub kind: RcTypeKind,
}

impl Type {
    pub fn new(token_range: Range<usize>, kind: RcTypeKind) -> Self {
        Type { token_range, kind }
    }

    pub(crate) fn new_empty_range(kind: RcTypeKind) -> Self {
        Type {
            token_range: 0..0,
            kind,
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

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
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

fn fmt_typekind_exit(ty: WeakTypeKind) -> String {
    match &*ty.upgrade().unwrap().borrow() {
        TypeKind::Func(func) => func.name.clone(),
        TypeKind::Struct(struct_) => struct_.name.clone(),
        other => format!("{}", other),
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", *self.kind.borrow())
    }
}
