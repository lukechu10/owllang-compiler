use owllang_lexer::TokenKind;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Serialize, Deserialize, Debug)]
/// Represents an expression.
pub struct Expr {
    #[serde(flatten)]
    pub kind: ExprKind,
}
impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self { kind }
    }
}
