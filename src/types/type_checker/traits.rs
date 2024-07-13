use crate::types::{IdentifierToken, WeakType};

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub methods: Vec<(IdentifierToken, WeakType)>,
}

impl PartialEq for Trait {
    fn eq(&self, other: &Self) -> bool {
        let mut result = self.name == other.name && self.methods.len() == other.methods.len();

        if !result {
            return false;
        };

        for i in 0..self.methods.len() {
            result = result && self.methods[i].0 == other.methods[i].0;
        }

        result
    }
}
