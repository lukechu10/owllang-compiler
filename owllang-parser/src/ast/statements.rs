use crate::ast::expressions::Expr;
use crate::traits::StatementAST;

#[derive(Debug)]
pub struct CompilationUnitAST {
    /// The name of the entry point file.
    pub entry_file_name: String,
    /// A compilation unit may only contain functions as top level statements.
    pub functions: Vec<FnStatementAST>,
}
impl CompilationUnitAST {
    pub fn new(entry_file_name: String, functions: Vec<FnStatementAST>) -> Self {
        Self {
            entry_file_name,
            functions,
        }
    }
}
impl StatementAST for CompilationUnitAST {}

/// Represents a block statement.
#[derive(Debug)]
pub struct BlockStatementAST {
    pub statements: Vec<Box<dyn StatementAST>>,
}
impl BlockStatementAST {
    pub fn new(statements: Vec<Box<dyn StatementAST>>) -> Self {
        Self { statements }
    }
}
impl StatementAST for BlockStatementAST {}

/// Represents a function prototype. Used for both function definitions and external function declarations.
#[derive(Debug)]
pub struct PrototypeAST {
    /// A `Vec` of argument names.
    pub arguments: Vec<String>,
    pub fn_identifier: String,
}
impl PrototypeAST {
    pub fn new(fn_identifier: String, arguments: Vec<String>) -> Self {
        Self {
            fn_identifier,
            arguments,
        }
    }
}
impl StatementAST for PrototypeAST {}

/// Represents a function definition or external function declaration.
#[derive(Debug)]
pub struct FnStatementAST {
    pub prototype: PrototypeAST,
    /// True if the function is an external function (declared with keyword `extern`).
    pub is_extern: bool,
    /// `Some` if function is not an external function. Else `None`.
    pub body: Option<BlockStatementAST>,
}
impl FnStatementAST {
    pub fn new(prototype: PrototypeAST, body: BlockStatementAST) -> Self {
        Self {
            prototype,
            is_extern: false,
            body: Some(body),
        }
    }
}
impl StatementAST for FnStatementAST {}

#[derive(Debug)]
pub struct WhileStatementAST {}
impl StatementAST for WhileStatementAST {}

#[derive(Debug)]
pub struct ForStatementAST {}
impl StatementAST for ForStatementAST {}

#[derive(Debug)]
pub struct IfStatementAST {}
impl StatementAST for IfStatementAST {}

#[derive(Debug)]
pub struct LetStatementAST {
    pub identifier: String,
    pub initializer_value: Expr,
}
impl LetStatementAST {
    pub fn new(identifier: String, initializer_value: Expr) -> Self {
        Self {
            identifier,
            initializer_value,
        }
    }
}
impl StatementAST for LetStatementAST {}

#[derive(Debug)]
pub struct ReturnStatementAST {
    pub ret_value: Expr,
}
impl ReturnStatementAST {
    pub fn new(ret_value: Expr) -> Self {
        Self { ret_value }
    }
}
impl StatementAST for ReturnStatementAST {}

#[derive(Debug)]
pub struct ExprStatementAST {
    pub expression: Expr,
}
impl ExprStatementAST {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}
impl StatementAST for ExprStatementAST {}
