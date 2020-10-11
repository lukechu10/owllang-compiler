//! Parsing logic. Transforms `Token`s into abstract syntax tree nodes.

use crate::ast::{expressions::*, statements::*};
use owlc_error::{Error, ErrorReporter};
use owlc_span::{BytePos, SourceFile};
use owlc_lexer::{Lexer, OpPrecedence, Token, TokenKind};
use std::iter::Peekable;
use std::rc::Rc;

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
            loc: BytePos(0).to(BytePos(1)),
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
        let err = Error::new(self.src.name.to_string(), self.current_token.loc, message);

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
                    loc: self.current_token.loc,
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
                loc: self.current_token.loc,
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
}

impl<'a> Parser<'a> {
    /// User can input both fn definitions and statements / expressions in the repl prompt.
    pub fn parse_repl_input(&mut self) -> Option<Stmt> {
        match self.current_token.kind {
            TokenKind::EndOfFile => None,
            TokenKind::KeywordFn | TokenKind::KeywordExtern => {
                let func = self.parse_fn_declaration();
                Some(func)
            }
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
                let expr_statement = Stmt::new(StmtKind::ExprSemi { expr });
                Some(expr_statement)
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
            TokenKind::KeywordWhile => {
                let while_stmt = self.parse_while_stmt();
                while_stmt
            }
            TokenKind::KeywordIf => {
                let if_else_stmt = self.parse_if_else_stmt();
                if_else_stmt
            }
            TokenKind::PuncOpenBrace => {
                let block = self.parse_block_statement();
                Stmt::new(StmtKind::Block(block))
            }
            _ => {
                // try to parse expression statement
                let expr = self.parse_expression();
                self.expect_and_eat_tok(TokenKind::PuncSemi);
                let expr_statement = Stmt::new(StmtKind::ExprSemi { expr });
                expr_statement
            }
        }
    }

    fn parse_return_statement(&mut self) -> Stmt {
        self.expect_and_eat_tok(TokenKind::KeywordReturn);
        let value = self.parse_expression();
        Stmt::new(StmtKind::Return { value })
    }

    fn parse_let_statement(&mut self) -> Stmt {
        self.expect_and_eat_tok(TokenKind::KeywordLet);
        let iden = self.expect_and_eat_iden_tok();
        self.expect_and_eat_tok(TokenKind::OpEquals);
        let initializer = self.parse_expression();
        Stmt::new(StmtKind::Let { iden, initializer })
    }

    /// Parses any valid expression.
    fn parse_expression(&mut self) -> Expr {
        let lhs = self.parse_primary_expression();
        self.parse_binary_expr_rhs(lhs, OpPrecedence::Expression as i8)
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
            TokenKind::LiteralInt(_) => {
                let literal_ast = self.parse_int_literal();
                literal_ast
            }
            TokenKind::Identifier(_) => self.parse_identifier_or_call_expr(),
            _ => {
                self.emit_err_at_current_tok(format!(
                    "Expected an expression but found {} token.",
                    self.current_token.kind
                ));
                Expr::new(ExprKind::Literal(0))
            }
        }
    }

    /// Returns the parsed `IntLiteral` token or `0` if bad match.
    fn parse_int_literal(&mut self) -> Expr {
        if let TokenKind::LiteralInt(num) = self.current_token.kind {
            self.eat_token(); // eat int literal token
            Expr::new(ExprKind::Literal(num))
        } else {
            self.emit_err_at_current_tok(format!(
                "Expected {} token but found {} token.",
                self.current_token.kind,
                TokenKind::LiteralInt(0) // dummy value for generating message
            ));
            Expr::new(ExprKind::Literal(0))
        }
    }

    /// Returns a `IdentifierExprAST` or `CallExprAST`, depending on the scenario.
    fn parse_identifier_or_call_expr(&mut self) -> Expr {
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
            Expr::new(ExprKind::FuncCall {
                callee: identifier,
                args,
            })
        } else {
            Expr::new(ExprKind::Identifier(identifier))
        }
    }

    fn parse_binary_expr_rhs(&mut self, lhs: Expr, prev_precedence: i8) -> Expr {
        let mut lhs_tmp = lhs;
        loop {
            let current_precedence = self.current_token.kind.precedence() as i8;

            // if this is a binary operator that binds at least as tightly as the current binary operator, consume it, otherwise we are done.
            // Example: "1 * 2 + 3"
            // current binary operator = '+'
            // previous binary operator = '*'
            // '+' does not bind as tight as '*' (has lower precedence)
            if current_precedence < prev_precedence {
                return lhs_tmp;
            }

            let binary_op_tok = self.eat_token(); // eat binary operator (precedence read into current_precedence)
            let mut rhs = self.parse_primary_expression(); // parse expression on rhs on operator

            // if binary operator binds less tightly with RHS than the operator after RHS, let the pending operator take RHS as its LHS.
            let next_precedence = self.current_token.kind.precedence() as i8;
            if current_precedence < next_precedence {
                rhs = self.parse_binary_expr_rhs(rhs, prev_precedence + 1);
            }

            lhs_tmp = Expr::new(ExprKind::BinaryExpr {
                lhs: Box::new(lhs_tmp),
                rhs: Box::new(rhs),
                op_type: binary_op_tok.kind,
            })
        }
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
            Stmt::new(StmtKind::Fn {
                proto,
                body: Some(body),
            })
        } else {
            self.expect_and_eat_tok(TokenKind::PuncSemi);
            Stmt::new(StmtKind::Fn { proto, body: None })
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
        self.expect_and_eat_tok(TokenKind::KeywordWhile);
        let condition = self.parse_expression();
        let body = self.parse_block_statement();

        Stmt::new(StmtKind::While { condition, body })
    }

    fn parse_if_else_stmt(&mut self) -> Stmt {
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

        Stmt::new(StmtKind::IfElse {
            if_condition,
            if_body,
            else_body,
        })
    }
}
