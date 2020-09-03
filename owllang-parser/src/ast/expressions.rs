use crate::traits::ExprAST;
use owllang_lexer::{OpPrecedence, TokenVal};
use std::fmt;

/// Represents a literal expression.
#[derive(Debug)]
pub struct LiteralExprAST<T>
where
    T: fmt::Debug,
{
    pub value: T,
}
impl<T> ExprAST for LiteralExprAST<T> where T: fmt::Debug {}

/// Represents an identifier expression.
#[derive(Debug)]
pub struct IdentifierExprAST {
    pub identifier: String,
}
impl IdentifierExprAST {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}
impl ExprAST for IdentifierExprAST {}

/// Represents a call expression.
#[derive(Debug)]
pub struct CallExprAST {
    pub callee: String,
    pub arguments: Vec<Box<dyn ExprAST>>,
}
impl CallExprAST {
    pub fn new(callee: String, arguments: Vec<Box<dyn ExprAST>>) -> Self {
        Self { callee, arguments }
    }
}
impl ExprAST for CallExprAST {}

#[derive(Debug)]
pub struct BinaryExprAST {
    pub lhs: Box<dyn ExprAST>,
    pub rhs: Box<dyn ExprAST>,
    /// The operator of the binary expression. Only enum variants starting with `Op` are allowed.
    pub op_type: TokenVal,
}
impl BinaryExprAST {
    pub fn new(lhs: Box<dyn ExprAST>, rhs: Box<dyn ExprAST>, op_type: TokenVal) -> Self {
        debug_assert!(
            op_type.precedence() != OpPrecedence::Error,
            "Token in BinaryExprAST should be a valid operator token."
        );
        Self { lhs, rhs, op_type }
    }
}
impl ExprAST for BinaryExprAST {}
