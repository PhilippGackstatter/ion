use crate::types::{MethodHeader, ProtoArena, ProtoFunction, Prototype, PrototypeId, TokenRange};

use super::PrototypeKind;

#[derive(Debug, Clone)]
pub struct LocatedType {
    pub token_range: TokenRange,
    pub typ: Type,
}

impl LocatedType {
    pub fn new(token_range: impl Into<TokenRange>, typ: Type) -> Self {
        LocatedType {
            token_range: token_range.into(),
            typ,
        }
    }

    pub(crate) fn new_empty_range(typ: Type) -> Self {
        LocatedType {
            token_range: (0..0).into(),
            typ,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Prototype(PrototypeId),
    Adhoc(AdhocTypeKind),
}

impl Type {
    pub fn new_prototype(id: PrototypeId) -> Self {
        Self::Prototype(id)
    }

    pub fn new_adhoc(kind: AdhocTypeKind) -> Self {
        Self::Adhoc(kind)
    }

    pub fn prototype_id(&self) -> Option<PrototypeId> {
        if let Type::Prototype(id) = self {
            Some(*id)
        } else {
            None
        }
    }

    pub fn resolve(self, arena: &ProtoArena) -> Prototype {
        match self {
            Type::Prototype(id) => arena.get(id).clone(),
            Type::Adhoc(adhoc) => match adhoc {
                AdhocTypeKind::Method(header) => Prototype::new(PrototypeKind::Method(header)),
                AdhocTypeKind::Function(function) => {
                    Prototype::new(PrototypeKind::Function(function))
                }
            },
        }
    }
}

impl AsRef<Type> for LocatedType {
    fn as_ref(&self) -> &Type {
        &self.typ
    }
}

impl AsRef<Type> for Type {
    fn as_ref(&self) -> &Type {
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AdhocTypeKind {
    Method(MethodHeader),
    Function(ProtoFunction),
}

impl std::fmt::Display for AdhocTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // TODO: Align with PrototypeKind::Function display impl.
            AdhocTypeKind::Method(method) => f.write_str(method.name.as_str()),
            AdhocTypeKind::Function(function) => f.write_str(function.name.as_str()),
        }
    }
}
