#![allow(dead_code)]

pub mod ast;
pub mod parser;
mod syntax_error;
mod traits;

pub use syntax_error::SyntaxError;
