//! Resolving logic for symbols in the generated abstract syntax tree.
use owlc_error::{Error, ErrorReporter};
use owlc_parser::ast::{expressions::*, statements::*};
use owlc_parser::visitor::AstVisitor;
use owlc_span::BytePos;

/// Represents a resolved symbol (can be either variable type or function type).
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// Represents a local variable (can be created either with `let` or as part of a function declaration).
    Let { ident: String },
    /// Represents a function. `ident` is the identifier (name) of the function. `args_count` is the number of arguments that the function accepts.
    Fn { ident: String, args_count: u32 },
}

impl Symbol {
    pub fn matches(&self, symbol_ident: &str) -> bool {
        match self {
            Symbol::Let { ident } => symbol_ident == ident,
            Symbol::Fn {
                ident,
                args_count: _,
            } => symbol_ident == ident,
        }
    }
}

/// Represents a lexical scope. A new `Scope` should be created every time the set of visible symbols changes. This can happen at blocks and `let` statements.
/// For performance reasons, `Block` variant can contain multiple `Symbol`s for function arguments.
/// Example:
/// ```owllang
/// fn test(a, b) {     // A new Scope::Let is created containing 'test'. A new Scope::Block is created containing 'a', 'b'.
///     let x = 0;      // A new Scope::Let is created containing 'x'.
///     {               // A new Scope::Block is created containing nothing.
///         let y = x;  // A new Scope::Let is created containing 'y'.
///     }               // Scopes on scope_stack are popped until the last Scope::Block is reached.
/// }
/// ```
#[derive(Debug)]
pub enum Scope {
    /// Represents block scope (created with `{` and destroyed with `}`). Block scope can also contain declarations (e.g. function arguments).
    /// The `Vec` field represents all the new symbols that are visible within the block (e.g. function arguments).
    Block(Vec<Symbol>),
    /// Represents a scope created by a variable declaration (`let` keyword). The `String` field represents the identifier of the symbol.
    Let(Symbol),
}

impl Scope {
    /// Returns `Some(symbol)` if current `Scope` contains a symbol with identifier `ident`.
    /// A return value of `None` does not necessarily mean that the symbol is inaccessible. The symbol can still be found in a previous `Scope`.
    pub fn get_symbol(&self, symbol_ident: &str) -> Option<&Symbol> {
        match self {
            Scope::Block(symbols) => {
                for symbol in symbols {
                    if symbol.matches(symbol_ident) {
                        return Some(symbol); // match found
                    };
                }
                None // no match found
            }
            Scope::Let(symbol) => {
                if symbol.matches(symbol_ident) {
                    Some(symbol)
                } else {
                    None
                }
            }
        }
    }
}

/// Manages all the symbols that are currently visible.
#[derive(Debug)]
pub struct SymbolTable {
    /// Stack of `Scope`s. To resolve a symbol, pop a `Scope` until a `Scope` with identifier is found.
    scope_stack: Vec<Scope>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scope_stack: Vec::new(),
        }
    }

    /// Returns `Some(symbol)` if symbol with `ident` is found. Else returns `None`.
    /// # Params
    /// * `ident` - The identifier of the symbol to lookup.
    pub fn lookup(&self, ident: &str) -> Option<&Symbol> {
        // look for symbol, starting from top of stack and going in reverse
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.get_symbol(ident) {
                return Some(symbol);
            }
        }

        None
    }

    /// Pushes `scope` onto the stack.
    pub fn push_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope);
    }

    /// Returns `None` if no more `Scope`s on stack.
    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scope_stack.pop()
    }

    /// Pops `Scope`s until a `Scope::Block` variant is found. Returns `None` if no more `Scope`s on stack and a `Scope::Block` variant has not yet been found.
    /// This method will pop the `Scope::Block` itself.
    pub fn pop_until_block(&mut self) -> Option<&Scope> {
        loop {
            let top_scope_is_block = {
                let top_scope = self.scope_stack.last();
                match top_scope {
                    None => return None,
                    Some(scope) => match scope {
                        Scope::Let(_) => false,
                        Scope::Block(_) => true,
                    },
                }
            };

            self.pop_scope(); // remove top scope

            if top_scope_is_block {
                return self.scope_stack.last();
            }
        }
    }
}

/// Generate a map for resolving symbols in the abstract syntax tree.
pub struct ResolverVisitor<'a> {
    pub symbols: SymbolTable,

    errors: &'a mut ErrorReporter,
}
impl<'a> ResolverVisitor<'a> {
    pub fn new(error_reporter: &'a mut ErrorReporter) -> Self {
        Self {
            symbols: SymbolTable::new(),
            errors: error_reporter,
        }
    }
}

impl<'a> AstVisitor for ResolverVisitor<'a> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Identifier(ident) => match self.symbols.lookup(ident) {
                Some(symbol) => match symbol {
                    Symbol::Fn {
                        ident: _,
                        args_count: _,
                    } => self.errors.report(
                        Error::new(
                            "repl".to_string(),
                            BytePos(0).to(BytePos(0)),
                            format!("Symbol {} is not a variable.", ident),
                        )
                        .with_help_hint(format!("A function exists with the same name. Call this function using '{}()'.", ident)),
                    ),
                    Symbol::Let { ident: _ } => {}
                },
                None => self.errors.report(Error::new(
                    "repl".to_string(),
                    BytePos(0).to(BytePos(0)),
                    format!("Identifier {} does not exist in current scope.", ident),
                )),
            },
            ExprKind::Literal(_) => {}
            ExprKind::FuncCall { callee, args } => {
                // visit function arguments
                for arg in args {
                    self.visit_expr(arg);
                }

                match self.symbols.lookup(callee) {
                    Some(symbol) => match symbol {
                        Symbol::Let { ident: _ } => {
                            self.errors.report(Error::new(
                                "repl".to_string(),
                                BytePos(0).to(BytePos(0)),
                                format!("Symbol {} is not a function.", callee),
                            ));
                        }
                        Symbol::Fn {
                            ident: _,
                            args_count,
                        } => {
                            if args.len() as u32 != *args_count {
                                self.errors.report(Error::new(
                                        "repl".to_string(),
                                        BytePos(0).to(BytePos(0)),
                                        format!("Function {} expected {} argument(s) but found {} argument(s).", callee, args_count, args.len()),
                                    ));
                            }
                        }
                    },
                    None => self.errors.report(Error::new(
                        "repl".to_string(),
                        BytePos(0).to(BytePos(0)),
                        format!("Function {} does not exist in current scope.", callee),
                    )),
                }
            }
            ExprKind::BinaryExpr {
                lhs,
                rhs,
                op_type: _,
            } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Block(block) => {
                self.visit_block(block);
            }
            StmtKind::Fn { proto, body } => self.visit_fn_stmt(proto, body),
            StmtKind::While { condition, body } => {
                self.visit_expr(condition);
                self.visit_block(body);
            }
            StmtKind::For => {}
            StmtKind::IfElse {
                if_condition,
                if_body,
                else_body,
            } => self.visit_if_else_stmt(if_condition, if_body, else_body),
            StmtKind::Let {
                iden,
                initializer: _,
            } => {
                if self.symbols.lookup(&iden).is_some() {
                    self.errors.report(Error::new(
                        "repl".to_string(),
                        BytePos(0).to(BytePos(0)),
                        format!("Variable {} is already declared.", iden),
                    ));
                } else {
                    let let_scope = Scope::Let(Symbol::Let {
                        ident: iden.clone(),
                    });
                    self.symbols.push_scope(let_scope);
                }
            }
            StmtKind::Return { value } => {
                self.visit_expr(value);
            }
            StmtKind::ExprSemi { expr } => {
                self.visit_expr(expr);
            }
            StmtKind::Noop => {}
        }
    }

    fn visit_fn_stmt(&mut self, proto: &FnProto, body: &Option<Block>) {
        // create new scope with function declaration.
        // This scope is pushed onto the scope_stack before the block scope to prevent it from being popped after exiting the function.
        let func_symbol = Symbol::Fn {
            ident: proto.iden.clone(),
            args_count: proto.args.len() as u32,
        };
        let let_scope = Scope::Let(func_symbol);
        self.symbols.push_scope(let_scope);

        // create new Scope::Block with function args.
        let symbols: Vec<Symbol> = proto
            .args
            .iter()
            .map(|arg| Symbol::Let { ident: arg.clone() })
            .collect();

        let block_scope = Scope::Block(symbols);
        self.symbols.push_scope(block_scope);

        // visit body
        // do not visit block as that will create a new block scope.
        match body {
            Some(body) => {
                for stmt in &body.stmts {
                    self.visit_stmt(stmt);
                }
            }

            None => {}
        }

        self.symbols.pop_until_block(); // remove all symbols created inside function and block scope itself. Does not remove symbol of function.
    }

    fn visit_block(&mut self, block: &Block) {
        // create empty Scope::Block
        let block_scope = Scope::Block(Vec::new());
        self.symbols.push_scope(block_scope);

        // visit statements
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        self.symbols.pop_until_block();
    }
}
