//! Makes sure main function has following signature: fn main().
//! Emits an error on any other signature.
//! This visitor should only be used when compiling a file and not when in REPL mode.

use owlc_error::{Error, ErrorReporter};
use owlc_parser::visitor::AstVisitor;
use owlc_span::BytePos;

/// Makes sure main function has following signature: fn main().
/// Emits an error on any other signature.
/// This visitor should only be used when compiling a file and not when in REPL mode.
pub struct MainFunctionVisitor<'a> {
    errors: &'a mut ErrorReporter,
}
impl<'a> MainFunctionVisitor<'a> {
    pub fn new(errors: &'a mut ErrorReporter) -> Self {
        Self { errors }
    }
}
impl<'a> AstVisitor for MainFunctionVisitor<'a> {
    fn visit_fn_proto(&mut self, ident: &str, args: &[String]) {
        if ident == "main" {
            // check if main function has correct signature
            if !args.is_empty() {
                self.errors.report(
                    Error::new(
                        "<repl>".to_string(),
                        BytePos(0).to(BytePos(1)),
                        "Invalid signature for main function. The entrypoint function does not accept any arguments.".to_string(),
                    )
                    .with_help_hint(
                        "Try changing the signature of the function to 'fn main()'.".to_string(),
                    ),
                )
            }
        }
    }
}
