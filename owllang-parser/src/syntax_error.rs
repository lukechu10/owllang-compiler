use std::fmt;

#[deprecated = "use owlc_error::ErrorReporter"]
#[derive(Debug)]
pub struct SyntaxError {
    /// The name of the file the error occurred in.
    pub file_name: String,
    pub row: u32,
    pub col: u32,
    /// The error message.
    pub message: String,
}
impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({}, {}): {}",
            self.file_name, self.row, self.col, self.message
        )
    }
}
