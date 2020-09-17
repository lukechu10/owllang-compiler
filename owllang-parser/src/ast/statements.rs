use crate::ast::expressions::Expr;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
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

/// Represents a block. e.g. `{...}`. A `Block` creates a new lexical scope. Variables declared inside the `Block` are not visible outside the `Block`.
/// # Grammar
/// ```ebnf
/// block = "{", statements, "}";
/// ```
#[derive(Serialize, Deserialize, Debug)]
pub struct Block {
    /// The statements inside the `Block`.
    pub stmts: Vec<Stmt>,
}

/// Represents a function prototype (not the function body). e.g. `fn f(x)`.
/// # Grammar
/// ```ebnf
/// fn proto param list = identifier, { ",", identifier };
/// fn proto = "fn", identifier, "(", fn proto param list, ")";
/// fn proto extern = "extern fn", identifier, "(", fn proto param list, ")";
/// ```
#[derive(Serialize, Deserialize, Debug)]
pub struct FnProto {
    /// The name of the arguments as declared in the function prototype.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<String>,
    /// The name of the function.
    pub iden: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum StmtKind {
    /// Wrapper around `Block`.
    Block(Block),
    /// Represents a function declaration (with definition if not `extern`). e.g. `fn f(x) { return 3; }`
    /// Field `body.kind` should always be variant `StmtKind::Block`.
    /// If field `body` is `None`, function is an extern function.
    /// # Grammar
    /// ```ebnf
    /// fn = ( fn proto, block ) | (fn proto extern, ";" );
    /// ```
    Fn {
        #[serde(flatten)]
        proto: FnProto,
        /// Field should be `None` if function is an `extern fn`.
        body: Option<Block>,
    },
    While,
    For,
    Let {
        iden: String,
        initializer: Expr,
    },
    Return {
        #[serde(flatten)]
        value: Expr,
    },
    /// Represents an expression with semi colon (expression with side effect).
    ExprSemi {
        #[serde(flatten)]
        expr: Expr,
    },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Stmt {
    #[serde(flatten)]
    pub kind: StmtKind,
}
impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Self { kind }
    }
}
