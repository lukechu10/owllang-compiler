//! Utilities for reporting errors during compilation.

use ansi_term::Color::*;
use ansi_term::Style;
use owlc_span::{SourceFile, Span};
use std::fmt;
use std::rc::Rc;

/// Represents a compile error.
#[derive(Debug, Clone)]
pub struct Error {
    /// The name of the file the error occurred in.
    pub file_name: String,
    /// The error message.
    pub message: String,
    /// Optional help message.
    pub help_hint: Option<String>,
    /// The span data / location of the error.
    pub loc: Span,
}

impl Error {
    pub fn new(file_name: String, loc: Span, message: String) -> Self {
        Self {
            file_name,
            loc,
            message,
            help_hint: None,
        }
    }

    pub fn with_help_hint(self, help_hint: String) -> Self {
        Self {
            help_hint: Some(help_hint),
            ..self
        }
    }
}

/// Handles all errors for a compilation unit.
/// Use `format!("{}", err_reporter);` to retrieve list of errors.
#[derive(Debug)]
pub struct ErrorReporter {
    src: Rc<SourceFile>,
    /// A list of reported errors
    errs: Vec<Error>,
}

impl ErrorReporter {
    /// Creates an empty `ErrorReporter` without any errors.
    pub fn new(src: Rc<SourceFile>) -> Self {
        Self {
            src,
            errs: Vec::new(),
        }
    }

    /// Add an error to the `ErrorReporter`.
    pub fn report(&mut self, err: Error) {
        self.errs.push(err);
    }

    /// Returns `true` if there are errors reported.
    pub fn has_errors(&self) -> bool {
        !self.errs.is_empty()
    }

    /// Consumes `error_reporter` and merges the errors into `self`.
    pub fn merge_from(&mut self, error_reporter: &ErrorReporter) {
        // self.errs.clone_from_slice(&error_reporter.errs);
        for err in &error_reporter.errs {
            self.errs.push(err.clone());
        }
    }
}

impl fmt::Display for ErrorReporter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in &self.errs {
            writeln!(
                f,
                "{}: {}",
                Red.bold().paint("error"),
                Style::default().bold().paint(&err.message)
            )?;
            writeln!(
                f,
                "{} {}:{}:{}",
                Blue.paint("   -->"),
                err.file_name,
                // Add 1 to location to provide 1 based index
                self.src.lookup_line(err.loc.lo).unwrap_or(0) + 1,
                self.src.lookup_col(err.loc.lo).unwrap_or(0) + 1,
            )?;

            if let Some(help_hint) = &err.help_hint {
                writeln!(
                    f,
                    "{} {}: {}",
                    Blue.paint("   -->"),
                    Blue.bold().paint("help"),
                    help_hint
                )?;
            }
        }
        Ok(())
    }
}
