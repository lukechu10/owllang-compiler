use crate::ast::expressions::{Expr, ExprKind};
use crate::ast::statements::{CompilationUnit, FnProto, Stmt, StmtKind};
use owllang_lexer::TokenKind;

pub trait AstVisitor {
    // expressions

    /// Visits any `Expr` node. Uses pattern matching to dispatch the correct visit method.
    /// This method should not be overridden unless custom behavior is needed.
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(val) => self.visit_literal_expr(val),
            ExprKind::Identifier(iden) => self.visit_identifier_expr(iden),
            ExprKind::FuncCall { callee, args } => self.visit_func_call(callee, args),
            ExprKind::BinaryExpr { lhs, rhs, op_type } => self.visit_bin_expr(lhs, rhs, op_type),
        }
    }
    fn visit_literal_expr(&mut self, _val: &i64) {}
    fn visit_identifier_expr(&mut self, _iden: &String) {}
    fn visit_func_call(&mut self, _callee: &String, _args: &Vec<Expr>) {}
    fn visit_bin_expr(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, _op_type: &TokenKind) {
        self.visit_expr(lhs);
        self.visit_expr(rhs);
    }

    // statements
    /// Visits any `Stmt` node. Uses pattern matching to dispatch the correct visit method.
    /// This method should not be overridden unless custom behavior is needed.
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Block { stmts } => self.visit_block_stmt(stmts),
            StmtKind::Fn { proto, body } => self.visit_fn_stmt(proto, body),
            StmtKind::While => self.visit_while_stmt(),
            StmtKind::For => self.visit_for_stmt(),
            StmtKind::Let { iden, initializer } => self.visit_let_stmt(iden, initializer),
            StmtKind::Return { value } => self.visit_return_stmt(value),
            StmtKind::ExprSemi { expr } => self.visit_expr_semi_stmt(expr),
        }
    }

    fn visit_compilation_unit(&mut self, compilation_unit: &CompilationUnit) {
        for func in &compilation_unit.functions {
            match &func.kind {
                StmtKind::Fn { proto, body } => self.visit_fn_stmt(proto, body),
                _ => unreachable!(),
            }
        }
    }

    fn visit_fn_proto(&mut self, _iden: &String, _args: &Vec<String>) {}
    fn visit_block_stmt(&mut self, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            self.visit_stmt(stmt);
        }
    }
    /// # Panics
    /// This method should panic if param `body.kind` is not a `StmtKind::Block` variant.
    fn visit_fn_stmt(&mut self, proto: &FnProto, body: &Option<Box<Stmt>>) {
        self.visit_fn_proto(&proto.iden, &proto.args);
        match body {
            Some(b) => match &b.kind {
                StmtKind::Block { stmts } => self.visit_block_stmt(stmts),
                _ => unreachable!(),
            },
            None => {}
        }
    }

    fn visit_while_stmt(&mut self) {
        unimplemented!();
    }
    fn visit_for_stmt(&mut self) {
        unimplemented!();
    }

    fn visit_let_stmt(&mut self, _iden: &String, initializer: &Expr) {
        self.visit_expr(initializer);
    }

    fn visit_return_stmt(&mut self, val: &Expr) {
        self.visit_expr(val);
    }

    fn visit_expr_semi_stmt(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }
}
