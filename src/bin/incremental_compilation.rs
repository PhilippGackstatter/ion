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
use petgraph::{
    dot::{Config, Dot},
    graphmap::DiGraphMap,
    visit::Dfs,
};
use serde::de::{self, Visitor};

fn main() {
    let prog = ion::util::file_to_string(&std::env::args().nth(1).unwrap()).unwrap();
    Compiler::new().compile(prog);
}

const DEBUG_GRAPH_PATH: &str = "./ion_target/debug_graph.dot";
const PREVIOUS_PROGRAM_PATH: &str = "./ion_target/previous_program.io";
const CHECKER_CACHE_PATH: &str = "./ion_target/checker_cache.json";
const CHECKER_DEPS_PATH: &str = "./ion_target/checker_deps.json";

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

struct ChangeSet {
    changed: Vec<SymbolId>,
    removed: Vec<SymbolId>,
}

/// Returns the declarations that are not in both `previous` and `current`.
fn program_diff(mut previous: Program, current: &Program) -> ChangeSet {
    let mut changed = Vec::new();
    let mut removed = Vec::new();

    for (symbol, declaration) in current.iter() {
        if let Some(prev_decl) = previous.remove(symbol) {
            if &prev_decl != declaration {
                changed.push(*symbol);
            }
        }
    }

    for removed_symbol in previous.into_keys() {
        removed.push(removed_symbol);
    }

    ChangeSet { changed, removed }
}

struct TypeChecker {
    /// An directed edge from a symbol A to a symbol B means that B depends on A.
    /// This way we can use a DFS to start from A and find all dependents that are invalidated by a changed A.
    dependencies: DiGraphMap<SymbolId, ()>,
    type_check_cache: HashMap<SymbolId, Result<(), CompileError>>,
    symbol_table: HashMap<TypeName, RcType>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut type_checker = TypeChecker {
            dependencies: DiGraphMap::new(),
            type_check_cache: HashMap::new(),
            symbol_table: HashMap::new(),
        };

        type_checker.add_predefined_symbol(TypeName::STR, wrap_typekind_impl(TypeKind::Str));
        type_checker.add_predefined_symbol(TypeName::BOOL, wrap_typekind_impl(TypeKind::Bool));
        type_checker
            .add_predefined_symbol(TypeName::Integer, wrap_typekind_impl(TypeKind::Integer));
        type_checker.add_predefined_symbol(TypeName::VOID, wrap_typekind_impl(TypeKind::Void));
        type_checker.add_predefined_symbol(TypeName::Double, wrap_typekind_impl(TypeKind::Double));

        type_checker
    }

    pub fn store(&self) {
        std::fs::write(
            CHECKER_DEPS_PATH,
            serde_json::to_string_pretty(&self.dependencies).unwrap(),
        )
        .unwrap();
        std::fs::write(
            CHECKER_CACHE_PATH,
            serde_json::to_string_pretty(&self.type_check_cache).unwrap(),
        )
        .unwrap();
    }

    pub fn restore() -> Self {
        let mut checker = Self::new();
        if let Ok(deps) = std::fs::read_to_string(CHECKER_DEPS_PATH) {
            checker.dependencies = serde_json::from_str(&deps).unwrap();
        }
        if let Ok(cache) = std::fs::read_to_string(CHECKER_CACHE_PATH) {
            checker.type_check_cache = serde_json::from_str(&cache).unwrap();
        }
        checker
    }

    fn add_predefined_symbol(&mut self, name: impl Into<TypeName>, typ: RcType) {
        self.add_symbol(name, LocatedType::new_empty_range(typ))
            .expect("predefined types should not exist");
    }

    pub fn check(&mut self, program: &Program) -> Result<(), CompileError> {
        for declaration in program.values() {
            self.build_symbol_table(declaration)?;
        }
        Ok(())
    }

    fn build_symbol_table(&mut self, decl: &Declaration) -> Result<(), CompileError> {
        match decl {
            Declaration::StructDecl {
                identifier,
                fields: token_fields,
            } => {
                log::debug!("symbol struct {}", identifier);
                let symbol_id = identifier.into();

                if let Some(cached_type) = self.type_check_cache.get(&symbol_id) {
                    println!("cache hit for {identifier}");
                    return cached_type.clone();
                }
                println!("cache miss for {identifier}");
                let build_result = self.build_struct_symbol(symbol_id, identifier, token_fields);
                self.type_check_cache
                    .insert(symbol_id, build_result.clone());
                build_result
            }
            _ => panic!("unsupported"),
        }
    }

    fn build_struct_symbol(
        &mut self,
        symbol_id: SymbolId,
        identifier: &IdentifierToken,
        token_fields: &[(IdentifierToken, IdentifierToken)],
    ) -> Result<(), CompileError> {
        let mut fields = Vec::new();
        for (field_name, ty) in token_fields {
            let type_ref = Rc::downgrade(&self.lookup_symbol_type(symbol_id, ty)?);
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
        self.add_symbol(TypeName::from(identifier.name.to_owned()), st)?;
        Ok(())
    }

    fn lookup_symbol_type(
        &mut self,
        caller: SymbolId,
        symbol: &IdentifierToken,
    ) -> Result<RcType, CompileError> {
        self.dependencies.add_edge(symbol.into(), caller, ());

        let type_name = TypeName::from(symbol.as_str().to_owned());
        if let Some(symbol) = self.symbol_table.get(&type_name) {
            Ok(Rc::clone(symbol))
        } else {
            Err(CompileError::new_migration(
                symbol.range.into(),
                format!("Type {} not declared in this scope.", type_name),
            ))
        }
    }

    fn add_symbol(
        &mut self,
        name: impl Into<TypeName>,
        located_type: LocatedType,
    ) -> Result<&RcType, CompileError> {
        match self.symbol_table.entry(name.into()) {
            Entry::Vacant(vacant) => Ok(vacant.insert(located_type.typ)),
            Entry::Occupied(occupied) => Err(CompileError::new_migration(
                located_type.token_range.clone(),
                format!("Type {} is already declared in this scope.", occupied.key()),
            )),
        }
    }

    /// Invalidates the type check cache.
    ///
    /// 1. Collect all nodes that have changed, including those that need to be rechecked because one of their dependencies became invalid.
    /// 2. Iterate all changed nodes and remove them from the graph and from the type check cache.
    fn invalidate_cache(&mut self, change_set: ChangeSet) {
        println!("removed: {:?}", change_set.removed);
        println!("changed: {:?}", change_set.changed);

        let changed_nodes = change_set.changed.iter().chain(change_set.removed.iter());

        let mut implied_changes_nodes = Vec::new();

        for changed_node in changed_nodes {
            let mut dfs = Dfs::new(&self.dependencies, *changed_node);
            while let Some(dependency) = dfs.next(&self.dependencies) {
                println!("`{changed_node}` causes dependency `{dependency}` to become invalid");
                implied_changes_nodes.push(dependency);
            }
        }

        let all_changed_nodes = change_set
            .changed
            .into_iter()
            .chain(change_set.removed)
            .chain(implied_changes_nodes);

        for invalidated_symbol in all_changed_nodes {
            // We may try to remove nodes twice, so we don't need to check whether it was successful or not.
            if self.dependencies.remove_node(invalidated_symbol) {
                println!("{invalidated_symbol} removed from graph");
            }

            // Do not unwrap the inner result.
            self.type_check_cache.remove(&invalidated_symbol);
        }
    }
}

pub fn store_previous_program(program: &ProgramFile) {
    std::fs::write(PREVIOUS_PROGRAM_PATH, program).unwrap();
}

pub fn restore_previous_program() -> Option<ProgramFile> {
    std::fs::read_to_string(PREVIOUS_PROGRAM_PATH).ok()
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
        // Ensure the ion_target dir exists.
        std::fs::create_dir_all("./ion_target/").unwrap();
        let mut lexer = ion::lexer::Lexer::new();
        lexer.lex(&program);
        let mut parser = ion::parser::Parser::new(&lexer);
        let current_program = parser.parse().unwrap();

        let mut checker = TypeChecker::restore();
        let current_program = declarations_to_tree(current_program);

        let previous_program = restore_previous_program();
        if let Some(previous_program) = previous_program {
            let mut lexer = ion::lexer::Lexer::new();
            lexer.lex(&previous_program);
            let previous_program = ion::parser::Parser::new(&lexer).parse().unwrap();

            let diff = program_diff(declarations_to_tree(previous_program), &current_program);

            checker.invalidate_cache(diff);
        }

        checker.check(&current_program).unwrap();

        checker.store();
        store_previous_program(&program);

        let dot = format!(
            "{:?}",
            Dot::with_config(&checker.dependencies, &[Config::EdgeNoLabel])
        );
        std::fs::write(DEBUG_GRAPH_PATH, dot).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diffing() {
        let program1 = r#"
struct Vector
  x: i32
  y: i32

struct Player
  position: Vector
  health: i32

struct Sword
  strength: i32

struct Shield
  durability: i32
"#;

        let program2 = r#"
struct Vector
  x: str
  y: i32

struct Player
  position: Vector
  health: i32

struct Sword
  strength: i32
"#;
        let mut lexer = ion::lexer::Lexer::new();
        lexer.lex(program1);
        let mut parser = ion::parser::Parser::new(&lexer);
        let program1 = declarations_to_tree(parser.parse().unwrap());

        let mut lexer = ion::lexer::Lexer::new();
        lexer.lex(program2);
        let mut parser = ion::parser::Parser::new(&lexer);
        let program2 = declarations_to_tree(parser.parse().unwrap());

        let diff = program_diff(program1, &program2);

        assert_eq!(
            diff.changed.as_slice(),
            &[SymbolId::from(&IdentifierToken::new_debug("Vector"))]
        );

        assert_eq!(
            diff.removed.as_slice(),
            &[SymbolId::from(&IdentifierToken::new_debug("Shield"))]
        );
    }
}
