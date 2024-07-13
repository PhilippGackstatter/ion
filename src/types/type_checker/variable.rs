use crate::types::{LocatedType, MoveContext};

#[derive(Debug)]
pub struct Variable {
    pub identifier: String,
    pub scope_depth: u8,
    pub dtype: LocatedType,
    /// The context of the move, if any.
    pub move_context: Option<MoveContext>,
}

impl Variable {
    pub fn new(identifier: String, scope_depth: u8, dtype: LocatedType) -> Self {
        Variable {
            identifier,
            scope_depth,
            dtype,
            move_context: None,
        }
    }
}
