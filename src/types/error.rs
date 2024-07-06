use std::ops::Range;

#[derive(Debug)]
pub struct CompileError {
    /// The indices in the source string that are erroneous
    pub token_range: Range<usize>,
    /// The error message
    pub message: String,
}
