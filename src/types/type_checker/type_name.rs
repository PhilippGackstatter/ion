const STR: &str = "str";
const BOOL: &str = "bool";
const VOID: &str = "VOID";
const I32: &str = "i32";
const F32: &str = "f32";

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeName {
    STR,
    Integer,
    Double,
    BOOL,
    VOID,
    Custom(String),
}

impl TypeName {
    pub fn name(&self) -> &str {
        match self {
            TypeName::STR => STR,
            TypeName::BOOL => BOOL,
            TypeName::VOID => VOID,
            TypeName::Integer => I32,
            TypeName::Double => F32,
            TypeName::Custom(name) => name,
        }
    }
}

impl From<String> for TypeName {
    fn from(name: String) -> Self {
        match name.as_ref() {
            STR => Self::STR,
            BOOL => Self::BOOL,
            VOID => Self::VOID,
            I32 => Self::Integer,
            F32 => Self::Double,
            _ => Self::Custom(name),
        }
    }
}

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}
