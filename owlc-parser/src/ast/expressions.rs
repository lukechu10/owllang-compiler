use owlc_lexer::TokenKind;
use owlc_span::Span;
use serde::Serialize;

/// Internal representation for [`Expr`](struct.Expr.html).
#[derive(Serialize, Debug)]
pub enum ExprKind {
    /// Represents an int literal (internally represented using `i64`).
    Literal(i64),
    /// Represents an identifier expression.
    Identifier(String),
    /// Represents a function call expression.
    FuncCall { callee: String, args: Vec<Expr> },
    BinaryExpr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        /// The operator type of the `BinaryExpr`. Represented with a `TokenVal`. The value of the field should only be valid operator variants of `TokenVal`.
        op_type: TokenKind,
    },
}

/// Represents an expression. Expressions may or may not have a side effect.
/// # Example
/// ```owllang
/// 1 + 1 // no side effect here
/// println(3) // side effect (printing to stdout)
/// ```
#[derive(Serialize, Debug)]
pub struct Expr {
    #[serde(flatten)]
    pub kind: ExprKind,
    #[serde(skip_serializing)]
    pub span: Span,
}
