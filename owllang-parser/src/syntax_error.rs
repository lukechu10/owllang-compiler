pub struct SyntaxError {
    /// The name of the file the error occurred in.
    pub file_name: String,
    pub row: u32,
    pub col: u32,
    /// The error message.
    pub message: String,
}
