use crate::ast::expressions::Expr;
use crate::SyntaxError;

#[derive(Debug)]
pub struct CompilationUnit {
    /// The name of the entry point file.
    pub entry_file_name: String,
    /// A compilation unit may only contain functions as top level statements.
    /// This field should only include variant `StmtKind::Fn`.
    pub functions: Vec<Stmt>,
}
impl CompilationUnit {
    pub fn new(entry_file_name: String) -> Self {
        Self {
            entry_file_name,
            functions: Vec::new(),
        }
    }

    pub fn add_func(&mut self, func: Stmt) {
        self.functions.push(func);
    }
}

#[derive(Debug)]
pub struct FnProto {
    pub args: Vec<String>,
    pub iden: String,
}

#[derive(Debug)]
pub enum StmtKind {
    /// Represents code delimited by curly brackets '{' and '}'.
    Block {
        statements: Vec<Stmt>,
    },
    /// Represents a function declaration / definition.
    /// Field `body.kind` should always be variant `StmtKind::Block`.
    /// If field `body` is `None`, function is an extern function.
    Fn {
        proto: FnProto,
        body: Option<Box<Stmt>>,
    },
    While,
    For,
    Let {
        iden: String,
        initializer: Expr,
    },
    Return {
        value: Expr,
    },
    /// Represents an expression with semi colon (expression with side effect).
    ExprSemi {
        expr: Expr,
    },
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}
impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Self { kind }
    }
}
