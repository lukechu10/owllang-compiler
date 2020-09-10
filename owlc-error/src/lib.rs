//! Utilities for reporting errors during compilation.

use std::fmt;

/// Represents a compile error.
#[derive(Debug)]
pub struct Error {
    /// The name of the file the error occurred in.
    pub file_name: String,
    /// The error message
    pub message: String,
    /// The row of the error.
    pub row: u32,
    /// The column of the error.
    pub col: u32,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({}:{}): {}",
            self.file_name, self.row, self.col, self.message
        )
    }
}

/// Handles all errors for a compilation unit.
/// Use `format!("{}", err_reporter);` to retrieve list of errors.
pub struct ErrorReporter {
    /// A list of reported errors
    errs: Vec<Error>,
}

impl ErrorReporter {
    /// Creates an empty `ErrorReporter` without any errors.
    pub fn new() -> Self {
        Self { errs: Vec::new() }
    }

    /// Add an error to the `ErrorReporter`.
    pub fn report(&mut self, err: Error) {
        self.errs.push(err);
    }

    /// Returns `true` if there are errors reported.
    pub fn has_errors(&self) -> bool {
        self.errs.len() > 0
    }
}

impl fmt::Display for ErrorReporter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in &self.errs {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}
