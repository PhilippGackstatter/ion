use std::collections::HashMap;

use crate::types::WeakTypeKind;

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, WeakTypeKind)>,
    pub number_of_fields: usize,
    /// The traits that this struct implements.
    pub traits: HashMap<String, WeakTypeKind>,
}

impl Struct {
    pub fn new(
        name: String,
        fields: Vec<(String, WeakTypeKind)>,
        number_of_fields: usize,
        traits: HashMap<String, WeakTypeKind>,
    ) -> Self {
        Self {
            name,
            fields,
            number_of_fields,
            traits,
        }
    }

    /// Returns the index in the memory layout of the field identified by `field_name`.
    pub fn field_layout_index(&self, field_name: &str) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == field_name)
            .map(|(idx, _)| idx)
    }
}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        let mut result = self.name == other.name && self.fields.len() == other.fields.len();

        if !result {
            return false;
        };

        for i in 0..self.fields.len() {
            result = result && self.fields[i].0 == other.fields[i].0;
            result = result && self.fields[i].1.ptr_eq(&other.fields[i].1);
        }

        result
    }
}
