use crate::{
    types::{IdentifierToken, MethodHeader, Prototype, PrototypeKind, TokenRange},
    util::display_error,
};
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
    pub fn new_migration(token_range: TokenRange, message: String) -> Self {
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
    TypeAlreadyExists(TypeAlreadyExists),
    TraitMethodImplMissing(TraitMethodImplMissing),
    TraitMethodImplIncorrect(TraitMethodImplIncorrect),
    Moved(Moved),
    Other(CompileMigrationError),
}

impl CompilationErrorKind {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        match self {
            Other(migration) => migration.display(context),
            TypeAlreadyExists(err) => err.display(context),
            Moved(err) => err.display(context),
            TraitMethodImplMissing(err) => err.display(context),
            TraitMethodImplIncorrect(err) => err.display(context),
        }
    }
}

/// Error that is used temporarily while migrating away from string-based errors to enums.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileMigrationError {
    /// The indices in the source string that are erroneous
    pub token_range: TokenRange,
    /// The error message
    pub message: String,
}

impl CompileMigrationError {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        display_error(context.program, self.token_range.into(), &self.message)
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

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodImplMissing {
    /// The location of the impl Trait block where the method is missing.
    pub impl_block_location: TokenRange,
    /// The set of methods that are missing to make the impl Trait complete.
    pub missing_methods: Vec<String>,
}

impl TraitMethodImplMissing {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        let missing_methods = self.missing_methods.join(", ");
        let article = if self.missing_methods.len() == 1 {
            "is"
        } else {
            "are"
        };
        let missing_methods_err = format!(
            "This impl block is missing methods that the trait requires: {missing_methods} {article} missing",
        );

        display_error(
            context.program,
            self.impl_block_location.into(),
            &missing_methods_err,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodImplIncorrect {
    /// The token of the method that is incorrect.
    pub incorrect_method: IdentifierToken,
    /// The expected header of the method (i.e. the correct one).
    pub expected_method_header: MethodHeader,
}

impl TraitMethodImplIncorrect {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        // Construct a prototype so we can reuse its Display implementation.
        let expected_method_prototype =
            Prototype::new(PrototypeKind::Method(self.expected_method_header.clone()));
        let incorrect_methods_err = format!(
            "Method {} is incorrectly implemented. Expected: {}",
            self.incorrect_method.name, expected_method_prototype,
        );

        display_error(
            context.program,
            self.incorrect_method.range.into(),
            &incorrect_methods_err,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlreadyExists {
    /// The location of the duplicate type.
    pub duplicate_location: TokenRange,
    /// The name of the type that already exists.
    pub duplicate_name: String,
}

impl TypeAlreadyExists {
    pub fn display(&self, context: CompilationErrorContext<'_>) -> String {
        display_error(
            context.program,
            self.duplicate_location.into(),
            &format!("Type {} already exists", self.duplicate_name),
        )
    }
}
