use std::ops::Range;

// Wrapper around the kind to allow for easier extension later.
// Perhaps we want to add the file path here where the error occured or other fields.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    pub kind: CompilationErrorKind,
}

impl CompileError {
    pub fn new_migration(token_range: Range<usize>, message: String) -> Self {
        Self {
            kind: CompilationErrorKind::Other(CompileMigrationError {
                token_range,
                message,
            }),
        }
    }

    pub fn unwrap_migration(self) -> CompileMigrationError {
        match self.kind {
            CompilationErrorKind::Other(migration) => migration,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompilationErrorKind {
    Other(CompileMigrationError),
}

/// Error that is used temporarily while migrating away from string-based errors to enums.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileMigrationError {
    /// The indices in the source string that are erroneous
    pub token_range: Range<usize>,
    /// The error message
    pub message: String,
}
