// //! Resolving logic for symbols in the generated abstract syntax tree.

// use owllang_parser::ast::{
//     expressions::{Expr, ExprKind},
//     statements::Stmt,
// };
// use owllang_parser::SyntaxError;
// use owllang_parser::Visitor;
// use std::collections::HashSet;

// /// Represents a resolved symbol (created via a `let` statement, created via a `fn` declaration, or a variable declared as a function argument).
// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum ResolvedSymbol {
//     /// Represents a resolved variable (`let` statement or function argument).
//     Var(String),
//     /// Represents a function declaration (`fn` declaration).
//     Fn(String),
// }

// #[derive(Debug)]
// pub enum ScopeKind {
//     /// Represents block scope (created with `{` and destroyed with `}`).
//     Block,
//     /// Represents a scope created by a variable declaration (`let` keyword).
//     Let,
// }

// /// Represents a lexical scope. A new `Scope` is created every time the set of visible names changes. This can occur in many different places.
// /// Example:
// /// ```owllang
// /// fn test(a, b) {     // a new Scope is created containing 'a', 'b' and 'test' (for recursion) with kind Block.
// ///     let x = 0;      // a new Scope is created containing 'x' with kind Let.
// ///     {               // a new Scope is created containing nothing with kind Block.
// ///         let y = x;  // a new Scope is created containing 'y with kind Let.
// ///     }               // top 2 Scopes are popped off the Scope stack (pop stack until scope with kind Block is reached).
// /// }
// /// ```
// #[derive(Debug)]
// pub struct Scope {
//     /// The kind of the `Scope`.
//     pub kind: ScopeKind,
//     /// A `HashSet` of all the symbols introduced with this scope.
//     pub symbols: HashSet<ResolvedSymbol>,
// }

// /// Generate a map for resolving symbols in the abstract syntax tree.
// #[derive(Debug)]
// pub struct ResolverVisitor {
//     /// Stack of `Scope`s. To resolve a symbol, pop a `Scope` until a `Scope` with identifier is found.
//     scope_stack: Vec<Scope>,
// }
// impl ResolverVisitor {
//     pub fn new() -> Self {
//         Self {
//             scope_stack: Vec::new(),
//         }
//     }

//     /// Returns `true` if `symbol` is in current scope. Else returns `false`.
//     pub fn symbol_in_scope(&self, symbol: &ResolvedSymbol) -> bool {
//         // start from top of stack and go down
//         for scope in self.scope_stack.iter().rev() {
//             if scope.symbols.contains(symbol) {
//                 return true;
//             }
//         }

//         false
//     }
// }

// impl Visitor for ResolverVisitor {
//     fn visit_expr(&mut self, node: &Expr) -> Result<(), SyntaxError> {
//         match &node.kind {
//             ExprKind::Identifier(iden) => {
//                 // let let_scope = Scope {
//                 //     kind: ScopeKind::Let,
//                 //     symbols: HashSet::with
//                 // };
//                 // self.scope_stack.push();
//                 Ok(())
//             }
//             _ => Ok(()),
//         }
//     }

//     fn visit_stmt(&mut self, node: &Stmt) -> Result<(), SyntaxError> {
//         match &node.kind {
//             _ => Ok(()),
//         }
//     }
// }
