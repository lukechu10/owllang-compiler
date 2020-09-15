use crate::ast::expressions::{Expr, ExprKind};
use crate::ast::statements::{CompilationUnit, FnProto, Stmt, StmtKind};
use crate::ast::NodeData;
use owllang_lexer::TokenKind;

pub trait AstVisitor {
    // expressions

    /// Visits any `Expr` node. Uses pattern matching to dispatch the correct visit method.
    /// This method should not be overridden unless custom behavior is needed.
    fn visit_expr(&mut self, node: &NodeData, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(val) => self.visit_literal_expr(node, val),
            ExprKind::Identifier(iden) => self.visit_identifier_expr(node, iden),
            ExprKind::FuncCall { callee, args } => self.visit_func_call(node, callee, args),
            ExprKind::BinaryExpr { lhs, rhs, op_type } => {
                self.visit_bin_expr(node, lhs, rhs, op_type)
            }
        }
    }
    fn visit_literal_expr(&mut self, _node: &NodeData, _val: &i64) {}
    fn visit_identifier_expr(&mut self, _node: &NodeData, _iden: &String) {}
    fn visit_func_call(&mut self, _node: &NodeData, _callee: &String, _args: &Vec<Expr>) {}
    fn visit_bin_expr(
        &mut self,
        node: &NodeData,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        _op_type: &TokenKind,
    ) {
        self.visit_expr(node, lhs);
        self.visit_expr(node, rhs);
    }

    // statements
    /// Visits any `Stmt` node. Uses pattern matching to dispatch the correct visit method.
    /// This method should not be overridden unless custom behavior is needed.
    fn visit_stmt(&mut self, node: &NodeData, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Block { statements } => self.visit_block_stmt(node, statements),
            StmtKind::Fn { proto, body } => self.visit_fn_stmt(node, proto, body),
            StmtKind::While => self.visit_while_stmt(node),
            StmtKind::For => self.visit_for_stmt(node),
            StmtKind::Let { iden, initializer } => self.visit_let_stmt(node, iden, initializer),
            StmtKind::Return { value } => self.visit_return_stmt(node, value),
            StmtKind::ExprSemi { expr } => self.visit_expr_semi_stmt(node, expr),
        }
    }

    fn visit_compilation_unit(&mut self, node: &NodeData, compilation_unit: &CompilationUnit) {
        for func in &compilation_unit.functions {
            match &func.kind {
                StmtKind::Fn { proto, body } => self.visit_fn_stmt(node, proto, body),
                _ => unreachable!(),
            }
        }
    }

    fn visit_fn_proto(&mut self, _node: &NodeData, _iden: &String, _args: &Vec<String>) {}
    fn visit_block_stmt(&mut self, node: &NodeData, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            self.visit_stmt(node, stmt);
        }
    }
    /// # Panics
    /// This method should panic if param `body.kind` is not a `StmtKind::Block` variant.
    fn visit_fn_stmt(&mut self, node: &NodeData, proto: &FnProto, body: &Option<Box<Stmt>>) {
        self.visit_fn_proto(node, &proto.iden, &proto.args);
        match body {
            Some(b) => match &b.kind {
                StmtKind::Block { statements } => self.visit_block_stmt(node, statements),
                _ => unreachable!(),
            },
            None => {}
        }
    }

    fn visit_while_stmt(&mut self, _node: &NodeData) {
        unimplemented!();
    }
    fn visit_for_stmt(&mut self, _node: &NodeData) {
        unimplemented!();
    }

    fn visit_let_stmt(&mut self, node: &NodeData, _iden: &String, initializer: &Expr) {
        self.visit_expr(node, initializer);
    }

    fn visit_return_stmt(&mut self, node: &NodeData, val: &Expr) {
        self.visit_expr(node, val);
    }

    fn visit_expr_semi_stmt(&mut self, node: &NodeData, expr: &Expr) {
        self.visit_expr(node, expr);
    }
}
