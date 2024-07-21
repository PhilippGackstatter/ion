use std::{
    cell::RefCell,
    collections::{hash_map::Entry, BTreeMap, HashMap},
    fmt::Write,
    rc::Rc,
};

const SYMBOL_CHAR_LEN: usize = 20;

use ion::types::{
    CompileError, Declaration, IdentifierToken, LocatedType, RcType, Struct, Type, TypeKind,
    TypeName,
};
use serde::de::{self, Visitor};

fn main() {
    let prog = ion::util::file_to_string(&std::env::args().nth(1).unwrap()).unwrap();
    Compiler::new().compile(prog);
}

fn wrap_typekind_impl(kind: TypeKind) -> RcType {
    Rc::new(RefCell::new(Type::new(kind)))
}

/// A representation of a program symbol such as a struct, function or trait.
///
/// This should be a super cheaply copyable type since we use it a lot and put it in data structures.
/// In a production setting, this should be a u64 which is a hash of the symbol.
///
/// For debugging purposes this currently contains a fixed-length String, but using a stack-allocated array
/// to enable deriving Copy, which is a requirement of petgraph.
///
/// We should make two versions of this, one with the hash and one with the fixed-length string and toggle with a feature.
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct SymbolId([char; SYMBOL_CHAR_LEN]);

impl From<&str> for SymbolId {
    fn from(id_token: &str) -> Self {
        let mut chars = ['\0'; SYMBOL_CHAR_LEN];
        for (idx, c) in id_token.chars().take(SYMBOL_CHAR_LEN).enumerate() {
            chars[idx] = c;
        }
        SymbolId(chars)
    }
}

impl std::fmt::Display for SymbolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.0 {
            if c == '\0' {
                break;
            }
            f.write_char(c)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for SymbolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let field = format!("{}", self);
        f.debug_tuple("SymbolId").field(&field).finish()
    }
}

impl serde::Serialize for SymbolId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let field = format!("{}", self);
        serializer.serialize_str(&field)
    }
}

struct StringVisitor;

impl<'de> Visitor<'de> for StringVisitor {
    type Value = String;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v.to_owned())
    }
}

impl<'de> serde::Deserialize<'de> for SymbolId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer
            .deserialize_str(StringVisitor)
            .map(|string| SymbolId::from(string.as_str()))
    }
}

impl From<&IdentifierToken> for SymbolId {
    fn from(id_token: &IdentifierToken) -> Self {
        // let mut hasher = seahash::SeaHasher::new();
        // id_token.as_str().hash(&mut hasher);
        // SymbolId(hasher.finish())
        SymbolId::from(id_token.as_str())
    }
}

type ProgramFile = String;

type Program = BTreeMap<SymbolId, Declaration>;

struct TypeChecker {
    symbols_to_check: HashMap<SymbolId, Declaration>,
    symbol_table: HashMap<SymbolId, RcType>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut type_checker = TypeChecker {
            symbol_table: HashMap::new(),
            symbols_to_check: HashMap::new(),
        };

        type_checker.add_predefined_symbol(TypeName::STR, wrap_typekind_impl(TypeKind::Str));
        type_checker.add_predefined_symbol(TypeName::BOOL, wrap_typekind_impl(TypeKind::Bool));
        type_checker
            .add_predefined_symbol(TypeName::Integer, wrap_typekind_impl(TypeKind::Integer));
        type_checker.add_predefined_symbol(TypeName::VOID, wrap_typekind_impl(TypeKind::Void));
        type_checker.add_predefined_symbol(TypeName::Double, wrap_typekind_impl(TypeKind::Double));

        type_checker
    }

    fn add_predefined_symbol(&mut self, name: impl Into<TypeName>, typ: RcType) {
        self.add_symbol(name, LocatedType::new_empty_range(typ))
            .expect("predefined types should not exist");
    }

    pub fn check(&mut self, program: Program) -> Result<(), CompileError> {
        self.symbols_to_check = program.into_iter().collect();

        loop {
            if let Some(symbol) = self.symbols_to_check.keys().next().map(|sym| *sym) {
                // Hack to be able to call check_symbol. Fix somehow.
                let id_token = self
                    .symbols_to_check
                    .get(&symbol)
                    .expect("it should be present")
                    .identifier()
                    .clone();
                self.check_symbol(None, &id_token)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    pub fn check_symbol(
        &mut self,
        dependent: Option<SymbolId>,
        symbol: &IdentifierToken,
    ) -> Result<RcType, CompileError> {
        if let Some(dependent) = dependent {
            println!("{dependent} depends on {symbol}");
        }

        let err = match self.lookup_symbol_type(symbol) {
            Ok(symbol_type) => {
                return Ok(symbol_type);
            }
            Err(err) => err,
        };

        let declaration = self.symbols_to_check.remove(&symbol.into()).ok_or(err)?;

        self.check_decl(&declaration)
    }

    fn check_decl(&mut self, decl: &Declaration) -> Result<RcType, CompileError> {
        match decl {
            Declaration::StructDecl {
                identifier,
                fields: token_fields,
            } => {
                log::debug!("symbol struct {}", identifier);
                self.check_struct_symbol(identifier, token_fields)
            }
            _ => panic!("unsupported"),
        }
    }

    fn check_struct_symbol(
        &mut self,
        identifier: &IdentifierToken,
        token_fields: &[(IdentifierToken, IdentifierToken)],
    ) -> Result<RcType, CompileError> {
        let mut fields = Vec::new();
        for (field_name, field_type_name) in token_fields {
            let symbol_type = self.check_symbol(Some(identifier.into()), field_type_name)?;
            let type_ref = Rc::downgrade(&symbol_type);
            fields.push((field_name.name.clone(), type_ref));
        }

        let number_of_fields = fields.len();

        let st = LocatedType::new(
            identifier.range.into(),
            Rc::new(RefCell::new(Type::new(TypeKind::Struct(Struct {
                name: identifier.name.clone(),
                fields,
                number_of_fields,
            })))),
        );
        let symbol_type = Rc::clone(&st.typ);
        self.add_symbol(TypeName::from(identifier.name.to_owned()), st)?;

        Ok(symbol_type)
    }

    fn lookup_symbol_type(&mut self, token: &IdentifierToken) -> Result<RcType, CompileError> {
        let symbol = token.into();
        if let Some(symbol) = self.symbol_table.get(&symbol) {
            Ok(Rc::clone(symbol))
        } else {
            Err(CompileError::new_migration(
                token.range.into(),
                format!("Type {} not declared in this scope.", token.as_str()),
            ))
        }
    }

    fn add_symbol(
        &mut self,
        name: impl Into<TypeName>,
        located_type: LocatedType,
    ) -> Result<&RcType, CompileError> {
        match self.symbol_table.entry(SymbolId::from(name.into().name())) {
            Entry::Vacant(vacant) => Ok(vacant.insert(located_type.typ)),
            Entry::Occupied(occupied) => Err(CompileError::new_migration(
                located_type.token_range.clone(),
                format!("Type {} is already declared in this scope.", occupied.key()),
            )),
        }
    }
}

fn declarations_to_tree(program: Vec<Declaration>) -> Program {
    program
        .into_iter()
        .map(|decl| {
            let Declaration::StructDecl { identifier, .. } = &decl else {
                panic!("unsupported");
            };
            (SymbolId::from(identifier), decl)
        })
        .collect()
}

struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, program: ProgramFile) {
        let mut lexer = ion::lexer::Lexer::new();
        lexer.lex(&program);
        let mut parser = ion::parser::Parser::new(&lexer);
        let current_program = parser.parse().unwrap();

        let mut checker = TypeChecker::new();
        let current_program = declarations_to_tree(current_program);

        checker.check(current_program).unwrap();
    }
}
