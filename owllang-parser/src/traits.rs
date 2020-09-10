use crate::ast::expressions::*;
use crate::ast::statements::*;
use crate::SyntaxError;

pub trait Visitor {
    // ast nodes in expressions.rs
    fn visit_expr(&mut self, node: &Expr) -> Result<(), SyntaxError>;

    // ast nodes in statements.rs
    fn visit_stmt(&mut self, node: &Stmt) -> Result<(), SyntaxError>;
    fn visit_compilation_unit(&mut self, node: &CompilationUnit) -> Result<(), SyntaxError> {
        // visit functions in compilation unit
        for function in &node.functions {
            self.visit_stmt(function)?;
        }
        Ok(())
    }
}

pub trait Visitable {
    /// Allows the `visitor` to visit this node.
    /// # Panics
    /// This method should not panic. If an error occurs, the method should return `Err(SyntaxError)`.
    fn accept(&self, visitor: &mut dyn Visitor) -> Result<(), SyntaxError>;
}
