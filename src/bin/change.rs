use std::{
    cell::RefCell,
    collections::{hash_map::Entry, BTreeMap, HashMap},
    hash::{Hash, Hasher},
    rc::Rc,
};

use ion::types::{
    CompileError, Declaration, IdentifierToken, LocatedType, RcType, Struct, Type, TypeKind,
    TypeName,
};

fn main() {
    let prog = ion::util::file_to_string(&std::env::args().nth(1).unwrap()).unwrap();
    Compiler::new().compile(prog);
}

const PREVIOUS_PROGRAM_PATH: &str = "./ion_target/previous_program.io";
const CHECKER_CACHE_PATH: &str = "./ion_target/checker_cache.json";
const CHECKER_DEPS_PATH: &str = "./ion_target/checker_deps.json";

fn wrap_typekind_impl(kind: TypeKind) -> RcType {
    Rc::new(RefCell::new(Type::new(kind)))
}

// fn wrap_type_impl(typ: Type) -> RcType {
//     Rc::new(RefCell::new(typ))
// }

#[derive(
    Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
// struct SymbolId(u64);
struct SymbolId(String);

impl From<&IdentifierToken> for SymbolId {
    fn from(id_token: &IdentifierToken) -> Self {
        // let mut hasher = seahash::SeaHasher::new();
        // id_token.as_str().hash(&mut hasher);
        // SymbolId(hasher.finish())
        SymbolId(id_token.to_string())
    }
}

type ProgramFile = String;

type Program = BTreeMap<SymbolId, Declaration>;

struct ChangeSet {
    changed: Vec<SymbolId>,
    removed: Vec<SymbolId>,
}

// struct Parser {}

// impl Parser {
//     pub fn parse(&self, program: ProgramFile) -> Program {
//         todo!()
//     }

/// Returns the declarations that are not in both `previous` and `current`.
fn program_diff(mut previous: Program, current: &Program) -> ChangeSet {
    let mut changed = Vec::new();
    let mut removed = Vec::new();

    for (symbol, declaration) in current.iter() {
        if let Some(prev_decl) = previous.remove(symbol) {
            if &prev_decl != declaration {
                changed.push(symbol.clone());
            }
        }
    }

    for removed_symbol in previous.into_keys() {
        removed.push(removed_symbol);
    }

    ChangeSet { changed, removed }
}

struct TypeChecker {
    // An entry here means that if key changes, all the symbols in the vector need
    // to be type checked again / their cache invalidated.
    dependencies: HashMap<SymbolId, Vec<SymbolId>>,
    type_check_cache: HashMap<SymbolId, Result<(), CompileError>>,
    symbol_table: HashMap<TypeName, RcType>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut type_checker = TypeChecker {
            dependencies: HashMap::new(),
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
                let build_result =
                    self.build_struct_symbol(symbol_id.clone(), identifier, token_fields);
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
            let type_ref = Rc::downgrade(&self.lookup_symbol_type(symbol_id.clone(), ty.into())?);
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
        match self.dependencies.entry(symbol.into()) {
            Entry::Occupied(mut occupied) => {
                if !occupied.get().contains(&caller) {
                    occupied.get_mut().push(caller);
                }
            }
            Entry::Vacant(vacant) => {
                vacant.insert(vec![caller]);
            }
        }

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

    fn invalidate_cache(&mut self, change_set: ChangeSet) {
        println!("removed: {:?}", change_set.removed);
        println!("changed: {:?}", change_set.changed);

        for symbol in change_set
            .changed
            .into_iter()
            .chain(change_set.removed.into_iter())
        {
            let deps = self.dependencies.remove(&symbol);

            // If the symbol does not exist, then nothing depended on it, so we only have to remove itself from the cache.
            let chain = IntoIterator::into_iter([symbol.clone()]).chain(deps.into_iter().flatten());

            for invalidated_symbol in chain {
                println!(
                    "{} causes {} to become invalid",
                    symbol.0, invalidated_symbol.0
                );
                // Do not unwrap the inner result.
                let _ = self
                    .type_check_cache
                    .remove(&invalidated_symbol)
                    .ok_or_else(|| {
                        Err::<(), _>(format!(
                            "the symbol {} should exist in the cache",
                            invalidated_symbol.0
                        ))
                    })
                    .unwrap();
            }
        }
    }
}

pub fn store_previous_program(program: &ProgramFile) {
    std::fs::write(PREVIOUS_PROGRAM_PATH, program).unwrap();
}

pub fn restore_previous_program() -> Option<ProgramFile> {
    std::fs::read_to_string(PREVIOUS_PROGRAM_PATH).ok()
}

pub fn declarations_to_tree(program: Vec<Declaration>) -> Program {
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
        lexer.lex(&program1);
        let mut parser = ion::parser::Parser::new(&lexer);
        let program1 = declarations_to_tree(parser.parse().unwrap());

        let mut lexer = ion::lexer::Lexer::new();
        lexer.lex(&program2);
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
