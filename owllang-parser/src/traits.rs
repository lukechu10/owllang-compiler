use crate::ast::expressions::*;
use crate::ast::statements::*;
use crate::SyntaxError;
use std::fmt::Debug;

pub trait Visitor {
    // ast nodes in expressions.rs
    fn visit_expr(&mut self, node: &Expr) -> Result<(), SyntaxError>;

    // ast nodes in statements.rs
    fn visit_compilation_unit(&mut self, node: &CompilationUnitAST) -> Result<(), SyntaxError>;
    fn visit_block(&mut self, node: &BlockStatementAST) -> Result<(), SyntaxError>;
    fn visit_prototype(&mut self, node: &PrototypeAST) -> Result<(), SyntaxError>;
    fn visit_fn_statement(&mut self, node: &FnStatementAST) -> Result<(), SyntaxError>;
    fn visit_let_statement(&mut self, node: &LetStatementAST) -> Result<(), SyntaxError>;
    fn visit_return_statement(&mut self, node: &ReturnStatementAST) -> Result<(), SyntaxError>;
    fn visit_expr_statement(&mut self, node: &ExprStatementAST) -> Result<(), SyntaxError>;
}

pub trait Visitable {
    /// Allows the `visitor` to visit this node.
    /// # Panics
    /// This method should not panic. If an error occurs, the method should return `Err(SyntaxError)`.
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError>;
}

impl Visitable for CompilationUnitAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_compilation_unit(self)
    }
}
impl Visitable for BlockStatementAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_block(self)
    }
}
impl Visitable for PrototypeAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_prototype(self)
    }
}
impl Visitable for FnStatementAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_fn_statement(self)
    }
}
impl Visitable for WhileStatementAST {
    fn accept(&self, _visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        unimplemented!()
    }
}
impl Visitable for ForStatementAST {
    fn accept(&self, _visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        unimplemented!()
    }
}
impl Visitable for IfStatementAST {
    fn accept(&self, _visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        unimplemented!()
    }
}
impl Visitable for LetStatementAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_let_statement(self)
    }
}
impl Visitable for ReturnStatementAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_return_statement(self)
    }
}
impl Visitable for ExprStatementAST {
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError> {
        visitor.visit_expr_statement(self)
    }
}

pub trait ASTNode: Visitable {}

pub trait StatementAST: Debug + Visitable {}
impl ASTNode for dyn StatementAST {}
