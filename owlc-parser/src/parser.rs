//! Parsing logic. Transforms `Token`s into abstract syntax tree nodes.

use crate::ast::{expressions::*, statements::*};
use owlc_error::{Error, ErrorReporter};
use owlc_lexer::{Lexer, Token, TokenKind};
use owlc_span::{BytePos, SourceFile};
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug)]
pub struct Parser<'a> {
    src: Rc<SourceFile>,
    lexer: Peekable<&'a mut Lexer<'a>>,
    current_token: Token,

    errs: &'a mut ErrorReporter,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>, error_reporter: &'a mut ErrorReporter) -> Self {
        let first_token = lexer.next().unwrap_or(Token {
            kind: TokenKind::EndOfFile,
            span: BytePos(0).to(BytePos(1)),
        });

        Self {
            src: Rc::clone(&lexer.src),
            lexer: lexer.peekable(),
            current_token: first_token,
            errs: error_reporter,
        }
    }

    /// Creates a new syntax error at the current token and adds it to the `ErrorReporter`.
    fn emit_err_at_current_tok(&mut self, message: String) {
        let err = Error::new(self.src.name.to_string(), self.current_token.span, message);

        self.errs.report(err);
    }

    /// Returns the token that was eaten. If `self.lexer` has no more `Token`s left, returns an `TokenKind::EndOfFile`.
    fn eat_token(&mut self) -> Token {
        let tmp_token = self.current_token.clone();
        match self.lexer.next() {
            Some(tok) => self.current_token = tok,
            None => {
                self.current_token = Token {
                    kind: TokenKind::EndOfFile,
                    span: self.current_token.span.hi.zero_width_span(),
                }
            }
        }
        tmp_token
    }

    /// Emits an error and constructs expected token even if bad match.
    fn expect_and_eat_tok(&mut self, expected: TokenKind) -> Token {
        let actual = self.eat_token();
        if actual.kind != expected {
            // emit error
            self.emit_err_at_current_tok(format!(
                "Expected {} token but found {} token.",
                expected, actual.kind
            ));

            Token {
                kind: expected,
                span: self.current_token.span,
            }
        } else {
            actual
        }
    }

    /// Returns the identifier `String` or `"0_err_iden"` value if current token is not an identifier.
    /// Error identifier starts with `'0'` to prevent conflicts with valid identifiers.
    fn expect_and_eat_iden_tok(&mut self) -> String {
        let actual = self.eat_token();
        if let TokenKind::Identifier(iden) = actual.kind {
            iden
        } else {
            self.emit_err_at_current_tok(format!(
                "Expected a {} token but found {} token.",
                TokenKind::Identifier("".to_string()),
                actual.kind
            ));
            "0_err_iden".to_string()
        }
    }

    pub fn record_current_pos(&self) -> BytePos {
        self.current_token.span.lo
    }
}

impl<'a> Parser<'a> {
    /// User can input both fn definitions and statements / expressions in the repl prompt.
    pub fn parse_repl_input(&mut self) -> Stmt {
        match self.current_token.kind {
            TokenKind::EndOfFile => Stmt {
                kind: StmtKind::Noop,
                span: self.record_current_pos().zero_width_span(),
            },
            TokenKind::KeywordFn | TokenKind::KeywordExtern => self.parse_fn_declaration(),
            // TokenKind::KeywordLet => {
            //     let let_statement = self.parse_let_statement();
            //     if self.current_token.kind == TokenKind::PuncSemi {
            //         self.expect_and_eat_tok(TokenKind::PuncSemi);
            //     }
            //     let_statement
            // }
            _ => {
                // try to parse expression statement
                let expr = self.parse_expression();
                // semi colon is optional in repl
                if self.current_token.kind == TokenKind::PuncSemi {
                    self.expect_and_eat_tok(TokenKind::PuncSemi);
                }
                Stmt {
                    span: expr.span.lo.to(self.record_current_pos()),
                    kind: StmtKind::ExprSemi { expr },
                }
            }
        }
    }

    pub fn parse_compilation_unit(&mut self) -> CompilationUnit {
        let mut compilation_unit = CompilationUnit::new(self.src.name.to_string());
        while self.current_token.kind != TokenKind::EndOfFile {
            let fn_declaration = self.parse_fn_declaration();
            compilation_unit.add_func(fn_declaration);
        }
        compilation_unit
    }

    fn parse_statement(&mut self) -> Stmt {
        match self.current_token.kind {
            TokenKind::KeywordReturn => {
                let ret_statement = self.parse_return_statement();
                self.expect_and_eat_tok(TokenKind::PuncSemi);
                ret_statement
            }
            TokenKind::KeywordLet => {
                let let_statement = self.parse_let_statement();
                self.expect_and_eat_tok(TokenKind::PuncSemi);
                let_statement
            }
            TokenKind::KeywordWhile => self.parse_while_stmt(),
            TokenKind::KeywordIf => self.parse_if_else_stmt(),
            TokenKind::PuncOpenBrace => {
                let lo = self.record_current_pos();
                let block = self.parse_block_statement();
                let hi = self.record_current_pos();
                Stmt {
                    kind: StmtKind::Block(block),
                    span: lo.to(hi),
                }
            }
            _ => {
                // try to parse expression statement
                let expr = self.parse_expression();
                self.expect_and_eat_tok(TokenKind::PuncSemi);
                Stmt {
                    span: expr.span.lo.to(self.record_current_pos()),
                    kind: StmtKind::ExprSemi { expr },
                }
            }
        }
    }

    fn parse_return_statement(&mut self) -> Stmt {
        let lo = self.record_current_pos();
        self.expect_and_eat_tok(TokenKind::KeywordReturn);
        let value = self.parse_expression();
        let hi = self.record_current_pos();
        Stmt {
            kind: StmtKind::Return { value },
            span: lo.to(hi),
        }
    }

    fn parse_let_statement(&mut self) -> Stmt {
        let lo = self.record_current_pos();
        self.expect_and_eat_tok(TokenKind::KeywordLet);
        let iden = self.expect_and_eat_iden_tok();
        self.expect_and_eat_tok(TokenKind::OpEquals);
        let initializer = self.parse_expression();
        let hi = self.record_current_pos();
        Stmt {
            kind: StmtKind::Let { iden, initializer },
            span: lo.to(hi),
        }
    }

    /// Parses any valid expression.
    fn parse_expression(&mut self) -> Expr {
        // 0 binding power to accept any expression
        self.parse_binary_expr_rhs(0)
    }

    /// Parses an atomic expression. Returns a `'0'` literal expression if bad match.
    fn parse_primary_expression(&mut self) -> Expr {
        match self.current_token.kind {
            TokenKind::PuncOpenParen => {
                self.expect_and_eat_tok(TokenKind::PuncOpenParen);
                // parse expression inside parenthesis.
                let expr_ast = self.parse_expression();
                self.expect_and_eat_tok(TokenKind::PuncCloseParen);
                expr_ast
            }
            TokenKind::LiteralInt(_) => self.parse_int_literal(),
            TokenKind::Identifier(_) => self.parse_identifier_or_call_expr(),
            _ => {
                self.emit_err_at_current_tok(format!(
                    "Expected an expression but found {} token.",
                    self.current_token.kind
                ));
                Expr {
                    kind: ExprKind::Literal(0),
                    span: self.record_current_pos().zero_width_span(),
                }
            }
        }
    }

    /// Returns the parsed `IntLiteral` token or `0` if bad match.
    fn parse_int_literal(&mut self) -> Expr {
        let lo = self.record_current_pos();
        if let TokenKind::LiteralInt(num) = self.current_token.kind {
            self.eat_token(); // eat int literal token
            let hi = self.record_current_pos();
            Expr {
                kind: ExprKind::Literal(num),
                span: lo.to(hi),
            }
        } else {
            self.emit_err_at_current_tok(format!(
                "Expected {} token but found {} token.",
                self.current_token.kind,
                TokenKind::LiteralInt(0) // dummy value for generating message
            ));
            Expr {
                kind: ExprKind::Literal(0),
                span: lo.zero_width_span(),
            }
        }
    }

    /// Returns a `IdentifierExprAST` or `CallExprAST`, depending on the scenario.
    fn parse_identifier_or_call_expr(&mut self) -> Expr {
        let lo = self.record_current_pos();
        let identifier = self.expect_and_eat_iden_tok();

        if self.current_token.kind == TokenKind::PuncOpenParen {
            self.expect_and_eat_tok(TokenKind::PuncOpenParen);
            // parse function call expression
            let mut args: Vec<Expr> = Vec::new();
            loop {
                if self.current_token.kind == TokenKind::PuncCloseParen {
                    // found end of argument list
                    self.expect_and_eat_tok(TokenKind::PuncCloseParen);
                    break;
                }

                let expr = self.parse_expression();
                args.push(expr);

                if self.current_token.kind == TokenKind::PuncComma {
                    self.expect_and_eat_tok(TokenKind::PuncComma);
                } else if self.current_token.kind != TokenKind::PuncCloseParen {
                    self.emit_err_at_current_tok(format!(
                        "Expected {} or {} token after expression in argument list but found {} token.",
                        TokenKind::PuncComma,
                        TokenKind::PuncCloseParen,
                        self.current_token.kind
                    ));
                    break;
                }
            }
            let hi = self.record_current_pos();
            Expr {
                kind: ExprKind::FuncCall {
                    callee: identifier,
                    args,
                },
                span: lo.to(hi),
            }
        } else {
            let hi = self.record_current_pos();
            Expr {
                kind: ExprKind::Identifier(identifier),
                span: lo.to(hi),
            }
        }
    }

    fn parse_binary_expr_rhs(&mut self, min_bp: i8) -> Expr {
        // handle prefix operators
        let prefix_bp = self.current_token.kind.prefix_binding_power();
        let mut lhs = match prefix_bp {
            // not a prefix operator
            ((), -1) => self.parse_primary_expression(),
            // prefix operator
            _ => {
                let lo = self.record_current_pos();

                let ((), right_bp) = prefix_bp;
                let prefix_op = self.eat_token(); // eat prefix operator
                let rhs = self.parse_binary_expr_rhs(right_bp);

                let hi = self.record_current_pos();
                match prefix_op.kind {
                    TokenKind::OpPlus => rhs,
                    // build as 0 - x
                    TokenKind::OpMinus => Expr {
                        kind: ExprKind::BinaryExpr {
                            lhs: Box::new(Expr {
                                kind: ExprKind::Literal(0),
                                span: lo.zero_width_span(), // not an error but 0 does not actually appear in source.
                            }),
                            rhs: Box::new(rhs),
                            op_type: TokenKind::OpMinus,
                        },
                        span: lo.to(hi),
                    },
                    _ => unreachable!("Invalid prefix operator."),
                }
            }
        };

        loop {
            let (left_bp, right_bp) = self.current_token.kind.infix_binding_power();

            // stop parsing
            if left_bp < min_bp {
                break;
            }
            let binop = self.eat_token(); // eat bin op

            let rhs = self.parse_binary_expr_rhs(right_bp);

            /*
            Record positions for diagnostics
                  lhs              hi
            lhs.lo   lhs.hi   hi.lo  hi.hi
            */
            let lo = lhs.span.lo;
            let hi = rhs.span.hi;

            lhs = Expr {
                kind: ExprKind::BinaryExpr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op_type: binop.kind,
                },
                span: lo.to(hi),
            }
        }

        lhs
    }

    fn parse_fn_prototype(&mut self) -> FnProto {
        self.expect_and_eat_tok(TokenKind::KeywordFn);
        let iden = self.expect_and_eat_iden_tok();
        self.expect_and_eat_tok(TokenKind::PuncOpenParen);

        // parse argument list
        let mut args: Vec<String> = Vec::new();
        loop {
            if self.current_token.kind == TokenKind::PuncCloseParen {
                self.expect_and_eat_tok(TokenKind::PuncCloseParen);
                break; // exit loop
            }
            // parse argument identifier
            let arg_iden = self.expect_and_eat_iden_tok();
            args.push(arg_iden);

            if self.current_token.kind == TokenKind::PuncComma {
                self.expect_and_eat_tok(TokenKind::PuncComma);
            } else if self.current_token.kind != TokenKind::PuncCloseParen {
                self.emit_err_at_current_tok(format!(
                    "Expected {} or {} token after identifier in argument list but found {} token.",
                    TokenKind::PuncComma,
                    TokenKind::PuncCloseParen,
                    self.current_token.kind
                ));
                break;
            }
        }

        FnProto { args, iden }
    }

    fn parse_fn_declaration(&mut self) -> Stmt {
        let lo = self.record_current_pos();
        let is_extern = {
            if self.current_token.kind == TokenKind::KeywordExtern {
                self.eat_token(); // eat 'extern' keyword
                true
            } else {
                false
            }
        };

        let proto = self.parse_fn_prototype();

        if !is_extern {
            let body = self.parse_block_statement();
            let hi = self.record_current_pos();
            Stmt {
                kind: StmtKind::Fn {
                    proto,
                    body: Some(body),
                },
                span: lo.to(hi),
            }
        } else {
            self.expect_and_eat_tok(TokenKind::PuncSemi);
            let hi = self.record_current_pos();
            Stmt {
                kind: StmtKind::Fn { proto, body: None },
                span: lo.to(hi),
            }
        }
    }

    fn parse_block_statement(&mut self) -> Block {
        self.expect_and_eat_tok(TokenKind::PuncOpenBrace);
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            if self.current_token.kind == TokenKind::PuncCloseBrace
                || self.current_token.kind == TokenKind::EndOfFile
            // if TokenKind::EndOfFile, self.expect_and_eat_tok will create an error.
            {
                self.expect_and_eat_tok(TokenKind::PuncCloseBrace);
                break;
            }
            let statement = self.parse_statement();
            stmts.push(statement);
        }

        Block { stmts }
    }

    fn parse_while_stmt(&mut self) -> Stmt {
        let lo = self.record_current_pos();
        self.expect_and_eat_tok(TokenKind::KeywordWhile);
        let condition = self.parse_expression();
        let body = self.parse_block_statement();
        let hi = self.record_current_pos();

        Stmt {
            kind: StmtKind::While { condition, body },
            span: lo.to(hi),
        }
    }

    fn parse_if_else_stmt(&mut self) -> Stmt {
        let lo = self.record_current_pos();
        self.expect_and_eat_tok(TokenKind::KeywordIf);
        let if_condition = self.parse_expression();
        let if_body = self.parse_block_statement();

        let else_body = if self.current_token.kind == TokenKind::KeywordElse {
            self.expect_and_eat_tok(TokenKind::KeywordElse);
            let else_body = self.parse_block_statement();
            Some(else_body)
        } else {
            None
        };
        let hi = self.record_current_pos();

        Stmt {
            kind: StmtKind::IfElse {
                if_condition,
                if_body,
                else_body,
            },
            span: lo.to(hi),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    /// Utility function for tests
    fn parse_str_as_repl(s: &str) -> Stmt {
        let source = Rc::new(SourceFile::new("<test>", s));
        let mut lexer_error_reporter = ErrorReporter::new(source.clone());
        let mut lexer = Lexer::with_source_file(&source, &mut lexer_error_reporter);
        let mut parser_error_reporter = ErrorReporter::new(source.clone());
        let mut parser = Parser::new(&mut lexer, &mut parser_error_reporter);

        let res = parser.parse_repl_input();

        let mut errors = ErrorReporter::new(source.clone());
        errors.merge_from(&lexer_error_reporter);
        errors.merge_from(&parser_error_reporter);
        assert!(!errors.has_errors());
        res
    }

    /// Utility function for tests
    fn parse_str_as_expr(s: &str) -> Expr {
        let source = Rc::new(SourceFile::new("<test>", s));
        let mut lexer_error_reporter = ErrorReporter::new(source.clone());
        let mut lexer = Lexer::with_source_file(&source, &mut lexer_error_reporter);
        let mut parser_error_reporter = ErrorReporter::new(source.clone());
        let mut parser = Parser::new(&mut lexer, &mut parser_error_reporter);

        let res = parser.parse_expression();

        let mut errors = ErrorReporter::new(source.clone());
        errors.merge_from(&lexer_error_reporter);
        errors.merge_from(&parser_error_reporter);
        assert!(!errors.has_errors());
        res
    }

    /// Utility function for tests
    fn parse_str_as_stmt(s: &str) -> Stmt {
        let source = Rc::new(SourceFile::new("<test>", s));
        let mut lexer_error_reporter = ErrorReporter::new(source.clone());
        let mut lexer = Lexer::with_source_file(&source, &mut lexer_error_reporter);
        let mut parser_error_reporter = ErrorReporter::new(source.clone());
        let mut parser = Parser::new(&mut lexer, &mut parser_error_reporter);

        let res = parser.parse_statement();

        let mut errors = ErrorReporter::new(source.clone());
        errors.merge_from(&lexer_error_reporter);
        errors.merge_from(&parser_error_reporter);
        assert!(!errors.has_errors());
        res
    }

    /// Utility function for tests
    fn parse_str_as_unit(s: &str) -> CompilationUnit {
        let source = Rc::new(SourceFile::new("<test>", s));
        let mut lexer_error_reporter = ErrorReporter::new(source.clone());
        let mut lexer = Lexer::with_source_file(&source, &mut lexer_error_reporter);
        let mut parser_error_reporter = ErrorReporter::new(source.clone());
        let mut parser = Parser::new(&mut lexer, &mut parser_error_reporter);

        let res = parser.parse_compilation_unit();

        let mut errors = ErrorReporter::new(source.clone());
        errors.merge_from(&lexer_error_reporter);
        errors.merge_from(&parser_error_reporter);
        assert!(!errors.has_errors());
        res
    }

    #[test]
    fn parse_basic_bin_op() {
        assert_debug_snapshot!(parse_str_as_expr("1 + 1"));
        assert_debug_snapshot!(parse_str_as_expr("a + b"));
    }

    #[test]
    fn parse_bad_bin_op() {
        parse_str_as_expr("1 ++ 1");
        parse_str_as_expr("1 -- 1");
    }

    #[test]
    fn parse_operator_precedence() {
        assert_debug_snapshot!(parse_str_as_expr("1 + 2 * 3"));
        assert_debug_snapshot!(parse_str_as_expr("1 * 2 + 3 * 4"));
        assert_debug_snapshot!(parse_str_as_expr("my.field"));
        assert_debug_snapshot!(parse_str_as_expr("my.nested.field")); // test right associativity

        assert_debug_snapshot!(parse_str_as_expr("foo = 1")); // assignment operator
        assert_debug_snapshot!(parse_str_as_expr("foo = foo + 1"));

        assert_debug_snapshot!(parse_str_as_expr("foo == 1"));
        assert_debug_snapshot!(parse_str_as_expr("foo < 1"));
        assert_debug_snapshot!(parse_str_as_expr("foo > 1"));
        assert_debug_snapshot!(parse_str_as_expr("foo <= 1"));
        assert_debug_snapshot!(parse_str_as_expr("foo >= 1"));
    }

    #[test]
    fn parse_unary_operator() {
        assert_debug_snapshot!(parse_str_as_expr("+1"));
        assert_debug_snapshot!(parse_str_as_expr("-1"));
        assert_debug_snapshot!(parse_str_as_expr("-1 + 2"));
        assert_debug_snapshot!(parse_str_as_expr("1 + (-2)"));
        assert_debug_snapshot!(parse_str_as_expr("1 + -2")); // also valid
        assert_debug_snapshot!(parse_str_as_expr("-my_var"));
    }

    #[test]
    fn parse_function_call() {
        assert_debug_snapshot!(parse_str_as_expr("my_function()"));
        assert_debug_snapshot!(parse_str_as_expr("my_function(10)"));
        assert_debug_snapshot!(parse_str_as_expr("my_function(10, a_variable, 2 + 2)"));
        assert_debug_snapshot!(parse_str_as_expr("func(nested_func())"));
        assert_debug_snapshot!(parse_str_as_expr("func(nested_func()) + 2"));
    }

    #[test]
    fn parse_let_statement() {
        assert_debug_snapshot!(parse_str_as_stmt("let x = 1;"));
        assert_debug_snapshot!(parse_str_as_stmt("let x = 1 + 1;"));
        assert_debug_snapshot!(parse_str_as_stmt("let x = func(2);"));
    }

    #[test]
    fn parse_if_else_statement() {
        assert_debug_snapshot!(parse_str_as_stmt(
            "if x {
                println(1);
                // do something
            }"
        )); // no else block
        assert_debug_snapshot!(parse_str_as_stmt(
            "if x {
                // do something
            } else {
                // do something else
            }"
        ));
    }

    #[test]
    #[should_panic]
    fn parse_alone_else_block() {
        parse_str_as_stmt(
            "// no if block
            else {
                // do something else
            }",
        );
    }

    #[test]
    fn parse_while_statement() {
        assert_debug_snapshot!(parse_str_as_stmt("while x < 10 { println(x); x = x + 1; }"));
        assert_debug_snapshot!(parse_str_as_stmt("while x < 10 { }")); // empty body
    }

    #[test]
    fn parse_fn_definition() {
        assert_debug_snapshot!(parse_str_as_unit(
            "fn test() {
                return 1;
            }"
        ));
        assert_debug_snapshot!(parse_str_as_unit(
            "fn test() {
                return 1 + 1;
            }"
        ));
        assert_debug_snapshot!(parse_str_as_unit(
            "fn recursion() {
                return recursion();
            }"
        ));
        assert_debug_snapshot!(parse_str_as_unit(
            "fn test() {
                let tmp = 1;
                return tmp;
            }"
        ));
        assert_debug_snapshot!(parse_str_as_unit(
            "fn test() {
                let i = 0;
                while i < 10 {
                    println(i);
                    i = i + 1;
                }
                return i;
            }"
        ));
        assert_debug_snapshot!(parse_str_as_unit(
            "fn comments() {
                // this is a comment
                // return 0;
                return 1;
            }"
        ));
    }

    #[test]
    #[should_panic]
    fn parse_fn_with_missing_paren() {
        parse_str_as_unit(
            "fn no_paren {
                return 0;
            }",
        );
    }

    #[test]
    fn parse_repl_input() {
        assert_debug_snapshot!(parse_str_as_repl("1 + 1;")); // accepts expressions (expression statement)
        assert_debug_snapshot!(parse_str_as_repl("1 + 1")); // semi colon is optional
        assert_debug_snapshot!(parse_str_as_repl("fn func() { return 1; }")); // function declarations / definition
        assert_debug_snapshot!(parse_str_as_repl("func()")); // function call

        assert_debug_snapshot!(parse_str_as_repl("")); // should be StmtKind::Noop
    }
}
