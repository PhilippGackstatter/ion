use std::ops::Range;

use crate::{types::TokenRange, util::display_error};
use CompilationErrorKind::*;

// Wrapper around the kind to allow for easier extension later.
// Perhaps we want to add the file path here where the error occured or other fields.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    pub kind: CompilationErrorKind,
}

pub struct CompilationErrorContext<'program> {
    pub(crate) program: &'program str,
}

impl<'program> CompilationErrorContext<'program> {
    pub fn new(program: &'program str) -> Self {
        Self { program }
    }
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

    pub fn new(kind: CompilationErrorKind) -> Self {
        Self { kind }
    }

    pub fn unwrap_migration(self) -> CompileMigrationError {
        match self.kind {
            Other(migration) => migration,
            _ => panic!("called unwrap on a non-migration error"),
        }
    }

    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        self.kind.display(context)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompilationErrorKind {
    Moved(Moved),
    Other(CompileMigrationError),
}

impl CompilationErrorKind {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        match self {
            Other(migration) => migration.display(context),
            Moved(moved) => moved.display(context),
        }
    }
}

/// Error that is used temporarily while migrating away from string-based errors to enums.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileMigrationError {
    /// The indices in the source string that are erroneous
    pub token_range: Range<usize>,
    /// The error message
    pub message: String,
}

impl CompileMigrationError {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        display_error(context.program, self.token_range.clone(), &self.message)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Moved {
    /// The name of the identifier that was moved.
    pub moved_identifier: String,
    /// The location where the value was originally moved.
    pub moved_at: TokenRange,
    /// The location where the value was tried to be used again.
    pub error_location: TokenRange,
    /// The name of the identifier that the value was moved into.
    pub moved_into: String,
}

impl Moved {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        let moved_at_error = format!(
            "{} was moved into {} here",
            self.moved_identifier, self.moved_into
        );
        let mut error = display_error(context.program, self.moved_at.into(), &moved_at_error);

        let use_after_move_error = format!("{} was used again here", self.moved_identifier);
        error.push_str(&display_error(
            context.program,
            self.error_location.into(),
            &use_after_move_error,
        ));

        error
    }
}
