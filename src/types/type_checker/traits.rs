#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
}

impl PartialEq for Trait {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
