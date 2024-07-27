use crate::types::IdentifierToken;

const STR: &str = "str";
const BOOL: &str = "bool";
const VOID: &str = "VOID";
const I32: &str = "i32";
const F32: &str = "f32";

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeName {
    Str,
    Integer,
    Double,
    Bool,
    Void,
    Custom(String),
}

impl TypeName {
    pub fn name(&self) -> &str {
        match self {
            TypeName::Str => STR,
            TypeName::Bool => BOOL,
            TypeName::Void => VOID,
            TypeName::Integer => I32,
            TypeName::Double => F32,
            TypeName::Custom(name) => name,
        }
    }
}

impl From<String> for TypeName {
    fn from(name: String) -> Self {
        match name.as_ref() {
            STR => Self::Str,
            BOOL => Self::Bool,
            VOID => Self::Void,
            I32 => Self::Integer,
            F32 => Self::Double,
            _ => Self::Custom(name),
        }
    }
}

impl From<IdentifierToken> for TypeName {
    fn from(name: IdentifierToken) -> Self {
        Self::from(name.name)
    }
}

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}
