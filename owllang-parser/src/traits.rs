use std::fmt::Debug;

pub trait ASTNode {}

pub trait StatementAST: Debug {}
impl ASTNode for dyn StatementAST {}

/// All ast nodes that represent expressions should implement this trait.
pub trait ExprAST: Debug {}
impl StatementAST for dyn ExprAST {}
impl ASTNode for dyn ExprAST {}
