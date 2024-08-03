use std::collections::HashMap;

use arenas::{Arena, EntryId};

use crate::types::{CompileError, IdentifierToken, Prototype, PrototypeId, TypeName};

pub struct ProtoArena {
    pub(crate) index: HashMap<TypeName, EntryId>,
    pub(crate) types: Arena<Prototype>,
}

impl ProtoArena {
    pub fn new() -> Self {
        Self {
            index: HashMap::new(),
            types: Arena::new(),
        }
    }

    pub fn insert(
        &mut self,
        name: IdentifierToken,
        prototype: Prototype,
    ) -> Result<PrototypeId, CompileError> {
        let type_name = TypeName::from(name.as_str().to_owned());
        println!("insert {type_name} as type {prototype}");
        if self.index.contains_key(&type_name) {
            return Err(CompileError::new_migration(
                name.range,
                format!("Type {name} already exists."),
            ));
        }

        let id = self.types.insert(prototype);
        debug_assert!(
            self.index.insert(type_name, id).is_none(),
            "we should have checked that no entry already exists"
        );

        Ok(id)
    }

    pub fn lookup(&self, name: &IdentifierToken) -> Result<PrototypeId, CompileError> {
        let type_name = TypeName::from(name.as_str().to_owned());

        let idx = match self.index.get(&type_name) {
            Some(idx) => idx,
            None => {
                return Err(CompileError::new_migration(
                    name.range,
                    format!("Type {name} does not exist."),
                ))
            }
        };

        Ok(*idx)
    }

    pub fn lookup_get(&self, name: &IdentifierToken) -> Result<&Prototype, CompileError> {
        let idx = self.lookup(name)?;

        Ok(self
            .types
            .get(idx)
            .expect("if the entry was in the index it should exist in the arena"))
    }

    pub fn get_mut(&mut self, prototype_id: PrototypeId) -> &mut Prototype {
        self.types
            .get_mut(prototype_id)
            .expect("if we index by id directly the type should always exist")
    }

    pub fn get(&self, prototype_id: PrototypeId) -> &Prototype {
        self.types
            .get(prototype_id)
            .expect("if we index by id directly the type should always exist")
    }
}

impl Default for ProtoArena {
    fn default() -> Self {
        Self::new()
    }
}
