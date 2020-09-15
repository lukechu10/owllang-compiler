//! Utilities for reporting errors during compilation.

use owlc_span::Span;
use std::fmt;

/// Represents a compile error.
#[derive(Debug)]
pub struct Error {
    /// The name of the file the error occurred in.
    pub file_name: String,
    /// The error message
    pub message: String,
    /// The span data / location of the error.
    pub loc: Span,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({}:{}): {}",
            // Add 1 to location to provide 1 based index
            self.file_name, self.loc.lo.0 + 1, self.loc.hi.0 + 1, self.message // TODO, compute row and col number
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

    /// Consumes `error_reporter` and merges the errors into `self`.
    pub fn merge_from(&mut self, error_reporter: &mut ErrorReporter) {
        self.errs.append(&mut error_reporter.errs);
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
