//! Owllang lexer implementation.

#![forbid(unsafe_code)]

mod lexer;
mod token;

pub use crate::lexer::Lexer;
pub use crate::token::{Token, TokenKind};
