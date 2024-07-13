use std::{
    collections::{hash_map::Entry, HashMap},
    hash::{Hash, Hasher},
};

use seahash::SeaHasher;

use crate::types::TokenRange;

// vvv This is how it should look like, so we can track the moves of structs on their individual fields.
// We want to be able to say that book.page.text was moved into x and book.page.number into y at the same time.
// Adapt the rest of the codebase to it.
#[derive(Debug)]
pub enum MoveContext {
    // Factor this out into a dedicated struct which is then also used inside StructMoveContent,
    // so there's a single type representing moved_into and moved_at.
    Basic(MoveDetails),
    Struct(StructMoves),
}

// Can be extended with enums later.
#[derive(Debug, PartialEq, Clone)]
pub struct MoveDetails {
    /// The identifier the variable was moved into.
    pub(crate) moved_into: String,
    /// The range of tokens where the move happened.
    pub(crate) moved_at: TokenRange,
}

impl MoveContext {
    pub fn new_basic(moved_into: String, moved_at: TokenRange) -> Self {
        Self::Basic(MoveDetails {
            moved_into,
            moved_at,
        })
    }

    pub fn new_struct(moved_into: String, moved_at: TokenRange, path: &[impl AsRef<str>]) -> Self {
        let mut struct_moves = StructMoves::new();
        struct_moves.mark_moved(path, moved_into, moved_at);
        Self::Struct(struct_moves)
    }
}

#[derive(Debug)]
pub struct StructMoveContent {
    pub children: Vec<String>,
    pub details: Option<MoveDetails>,
}

#[derive(Debug)]
pub struct StructMoves {
    tree: HashMap<u64, StructMoveContent>,
}

impl StructMoves {
    pub fn new() -> Self {
        Self {
            tree: HashMap::new(),
        }
    }

    pub fn mark_moved<T: AsRef<str>>(
        &mut self,
        path: &[T],
        moved_into: String,
        token_range: TokenRange,
    ) {
        let mut moved_into = Some(moved_into);
        for i in 0..path.len() {
            if i + 1 == path.len() {
                let path_hash = Self::hash_key(path);
                debug_assert!(!self.tree.contains_key(&path_hash));

                // Last entry in the path.
                self.tree.insert(
                    path_hash,
                    StructMoveContent {
                        children: vec![],
                        details: Some(MoveDetails {
                            moved_into: moved_into
                                .take()
                                .expect("we should only call this once per loop"),
                            moved_at: token_range,
                        }),
                    },
                );
            } else {
                let path_hash = Self::hash_key(&path[..=i]);
                let child = path[i + 1].as_ref().to_owned();

                let is_child_contained = {
                    let child_hash = Self::hash_key(&path[..=i + 1]);
                    self.tree.contains_key(&child_hash)
                };

                match self.tree.entry(path_hash) {
                    Entry::Occupied(mut content) => {
                        // debug_assert!(!content.get().children.contains(&child));
                        if !is_child_contained {
                            content.get_mut().children.push(child);
                        }
                    }
                    Entry::Vacant(content) => {
                        content.insert(StructMoveContent {
                            children: vec![child],
                            details: None,
                        });
                    }
                }
            }
        }
    }

    pub fn get(&self, path: &[&str]) -> Option<&StructMoveContent> {
        let hash = Self::hash_key(path);
        self.tree.get(&hash)
    }

    /// Find one field of the struct that caused a partial move of the entire struct.
    ///
    /// TODO: Expand later to find not just any, but all of the fields.
    pub fn find_move_reason(&self, identifier: &str) -> Option<MoveDetails> {
        let mut path = vec![identifier];
        loop {
            match self.get(path.as_slice()) {
                Some(found) => {
                    if let Some(details) = &found.details {
                        return Some(details.clone());
                    }
                    // Otherwise continue down the tree.
                    path.push(
                        found
                            .children
                            .first()
                            .expect("there should be at least one child"),
                    );
                }
                None => todo!(),
            }
        }
    }

    fn hash_key<T: AsRef<str>>(path: &[T]) -> u64 {
        let mut hasher = SeaHasher::new();
        for entry in path.iter() {
            entry.as_ref().hash(&mut hasher);
        }
        hasher.finish()
    }
}

impl Default for StructMoves {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct_moves() {
        let mut struct_moves = StructMoves::new();
        struct_moves.mark_moved(&["vector"], "ident".to_owned(), (10..22).into());
        struct_moves.mark_moved(&["book", "page", "text"], "ident".to_owned(), (0..2).into());
        struct_moves.mark_moved(
            &["book", "page", "color"],
            "ident".to_owned(),
            (3..7).into(),
        );

        assert!(struct_moves.get(&["vector"]).unwrap().children.is_empty());
        assert_eq!(
            struct_moves
                .get(&["vector"])
                .unwrap()
                .details
                .as_ref()
                .unwrap(),
            &MoveDetails {
                moved_into: "ident".to_owned(),
                moved_at: (10..22).into()
            }
        );

        assert_eq!(struct_moves.get(&["book"]).unwrap().children, &["page"]);
        assert_eq!(
            struct_moves.get(&["book", "page"]).unwrap().children,
            &["text", "color"]
        );
        assert_eq!(
            struct_moves
                .get(&["book", "page", "text"])
                .unwrap()
                .details
                .as_ref()
                .unwrap(),
            &MoveDetails {
                moved_into: "ident".to_owned(),
                moved_at: (0..2).into(),
            }
        );

        assert!(struct_moves.get(&["book", "page", "number"]).is_none());
        assert!(struct_moves.get(&["book", "length"]).is_none());
    }
}
