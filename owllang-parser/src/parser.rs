use crate::ast::{expressions::*, statements::*};
use crate::SyntaxError;
use owllang_lexer::{Lexer, OpPrecedence, Token, TokenVal};
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<&'a mut Lexer<'a>>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let first_token = lexer.next().unwrap();
        Self {
            lexer: lexer.peekable(),
            current_token: first_token,
        }
    }

    /// Creates a new syntax error at the current token.
    fn new_syntax_error_at_current_token(&self, message: String) -> SyntaxError {
        SyntaxError {
            file_name: "repl_tmp".to_string(),
            row: self.current_token.row,
            col: self.current_token.col,
            message,
        }
    }

    /// Returns the token that was eaten.
    fn eat_token(&mut self) -> Result<Token, SyntaxError> {
        let tmp_token = self.current_token.clone();
        match self.lexer.next() {
            Some(tok) => self.current_token = tok,
            None => {
                self.current_token = Token {
                    value: TokenVal::EndOfFile,
                    row: self.current_token.row,
                    col: self.current_token.col + 1, // 1 character after last token
                    len: 0,
                }
            }
        }
        Ok(tmp_token)
    }

    /// Returns a `Err(SyntaxError)` if the token does not match the expected token. Eats the token even if bad match.
    fn expect_and_eat_tok(&mut self, expected: TokenVal) -> Result<Token, SyntaxError> {
        let actual = self.eat_token()?;
        if actual.value != expected {
            Err(self.new_syntax_error_at_current_token(
                format!(
                    "Expected a {:?} token. Found a {:?} token.",
                    expected, actual.value
                )
                .to_string(),
            ))
        } else {
            Ok(actual)
        }
    }

    /// Returns the identifier `String` or a `SyntaxError` if bad match. Eats the token even if bad match.
    fn expect_and_eat_iden_tok(&mut self) -> Result<String, SyntaxError> {
        let actual = self.eat_token()?;
        if let TokenVal::Identifier(iden) = actual.value {
            Ok(iden)
        } else {
            Err(self.new_syntax_error_at_current_token(format!(
                "Expected a identifier token. Found a {:?} token",
                actual.value
            )))
        }
    }
}

impl<'a> Parser<'a> {
    /// User can input both fn definitions and statements / expressions in the repl prompt.
    pub fn parse_repl_input(&mut self) -> Result<Stmt, SyntaxError> {
        match self.current_token.value {
            TokenVal::KeywordFn => {
                let func = self.parse_fn_declaration()?;
                Ok(func)
            }
            TokenVal::KeywordLet => {
                let let_statement = self.parse_let_statement()?;
                if self.current_token.value == TokenVal::PuncSemi {
                    self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                }
                Ok(let_statement)
            }
            _ => {
                // try to parse expression statement
                let expr = self.parse_expression()?;
                // semi colon is optional in repl
                if self.current_token.value == TokenVal::PuncSemi {
                    self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                }
                let expr_statement = Stmt::new(StmtKind::ExprSemi { expr });
                Ok(expr_statement)
            }
        }
    }

    pub fn parse_compilation_unit(&mut self) -> Result<CompilationUnit, Vec<SyntaxError>> {
        let mut compilation_unit = CompilationUnit::new("entry".to_string());
        while self.current_token.value != TokenVal::EndOfFile {
            match self.parse_fn_declaration() {
                Ok(func) => compilation_unit.add_func(func),
                Err(err) => compilation_unit.add_err(err),
            }
        }
        Ok(compilation_unit)
    }

    fn parse_statement(&mut self) -> Result<Stmt, SyntaxError> {
        match self.current_token.value {
            TokenVal::KeywordReturn => {
                let ret_statement = self.parse_return_statement()?;
                self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                Ok(ret_statement)
            }
            TokenVal::KeywordLet => {
                let let_statement = self.parse_let_statement()?;
                self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                Ok(let_statement)
            }
            TokenVal::PuncOpenBrace => {
                let block = self.parse_block_statement()?;
                Ok(block)
            }
            _ => {
                // try to parse expression statement
                let expr = self.parse_expression()?;
                self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                let expr_statement = Stmt::new(StmtKind::ExprSemi { expr });
                Ok(expr_statement)
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::KeywordReturn)?;
        let value = self.parse_expression()?;
        Ok(Stmt::new(StmtKind::Return { value }))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::KeywordLet)?;
        let iden = self.expect_and_eat_iden_tok()?;
        self.expect_and_eat_tok(TokenVal::OpEquals)?;
        let initializer = self.parse_expression()?;
        Ok(Stmt::new(StmtKind::Let { iden, initializer }))
    }

    /// Parses any valid expression.
    fn parse_expression(&mut self) -> Result<Expr, SyntaxError> {
        let lhs = self.parse_primary_expression()?;
        self.parse_binary_expr_rhs(lhs, OpPrecedence::Expression as i8)
    }

    /// Atomic expression.
    fn parse_primary_expression(&mut self) -> Result<Expr, SyntaxError> {
        match self.current_token.value {
            TokenVal::PuncOpenParen => {
                self.expect_and_eat_tok(TokenVal::PuncOpenParen)?;
                // parse expression inside parenthesis.
                let expr_ast = self.parse_expression()?;
                self.expect_and_eat_tok(TokenVal::PuncCloseParen)?;
                Ok(expr_ast)
            }
            TokenVal::LiteralInt(_) => {
                let literal_ast = self.parse_int_literal()?;
                Ok(literal_ast)
            }
            TokenVal::Identifier(_) => self.parse_identifier_or_call_expr(),
            _ => Err(self.new_syntax_error_at_current_token(format!(
                "Unexpected {:?} token when expecting an expression.",
                self.current_token.value
            ))),
        }
    }

    fn parse_int_literal(&mut self) -> Result<Expr, SyntaxError> {
        if let TokenVal::LiteralInt(num) = self.current_token.value {
            self.eat_token()?; // eat int literal token
            Ok(Expr::new(ExprKind::Literal(num)))
        } else {
            Err(self.new_syntax_error_at_current_token("Expected an int literal.".to_string()))
        }
    }

    /// Returns a `IdentifierExprAST` or `CallExprAST`, depending on the scenario.
    fn parse_identifier_or_call_expr(&mut self) -> Result<Expr, SyntaxError> {
        let identifier: String = if let TokenVal::Identifier(iden) = &self.current_token.value {
            iden.clone()
        } else {
            return Err(
                self.new_syntax_error_at_current_token("Expected an identifier.".to_string())
            );
        };
        self.eat_token()?; // eat identifier token

        if self.current_token.value == TokenVal::PuncOpenParen {
            self.expect_and_eat_tok(TokenVal::PuncOpenParen)?;
            // parse function call expression
            let mut args: Vec<Expr> = Vec::new();
            loop {
                if self.current_token.value == TokenVal::PuncCloseParen {
                    self.expect_and_eat_tok(TokenVal::PuncCloseParen)?;
                    break;
                }
                let expr = self.parse_expression()?;
                args.push(expr);
                if self.current_token.value == TokenVal::PuncComma {
                    self.expect_and_eat_tok(TokenVal::PuncComma)?;
                } else if self.current_token.value != TokenVal::PuncCloseParen {
                    return Err(self.new_syntax_error_at_current_token(
                        "Expected a ',' or ')' token after expression in argument list."
                            .to_string(),
                    ));
                }
            }
            // Ok(Box::new(CallExprAST::new(identifier, args)))
            Ok(Expr::new(ExprKind::FuncCall {
                callee: identifier,
                args,
            }))
        } else {
            Ok(Expr::new(ExprKind::Identifier(identifier)))
        }
    }

    fn parse_binary_expr_rhs(
        &mut self,
        lhs: Expr,
        prev_precedence: i8,
    ) -> Result<Expr, SyntaxError> {
        let mut lhs_tmp = lhs;
        loop {
            let current_precedence = self.current_token.value.precedence() as i8;

            // if this is a binary operator that binds at least as tightly as the current binary operator, consume it, otherwise we are done.
            // Example: "1 * 2 + 3"
            // current binary operator = '+'
            // previous binary operator = '*'
            // '+' does not bind as tight as '*' (has lower precedence)
            if current_precedence < prev_precedence {
                return Ok(lhs_tmp);
            }

            let binary_op_tok = self.eat_token()?; // eat binary operator (precedence read into current_precedence)
            let mut rhs = self.parse_primary_expression()?; // parse expression on rhs on operator

            // if binary operator binds less tightly with RHS than the operator after RHS, let the pending operator take RHS as its LHS.
            let next_precedence = self.current_token.value.precedence() as i8;
            if current_precedence < next_precedence {
                rhs = self.parse_binary_expr_rhs(rhs, prev_precedence + 1)?;
            }

            lhs_tmp = Expr::new(ExprKind::BinaryExpr {
                lhs: Box::new(lhs_tmp),
                rhs: Box::new(rhs),
                op_type: binary_op_tok.value,
            })
        }
    }

    fn parse_fn_prototype(&mut self) -> Result<FnProto, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::KeywordFn)?;
        let iden = self.expect_and_eat_iden_tok()?;
        self.expect_and_eat_tok(TokenVal::PuncOpenParen)?;

        // parse argument list
        let mut args: Vec<String> = Vec::new();
        loop {
            if self.current_token.value == TokenVal::PuncCloseParen {
                self.expect_and_eat_tok(TokenVal::PuncCloseParen)?;
                break; // exit loop
            }
            // parse argument identifier
            let arg_iden = self.expect_and_eat_iden_tok()?;
            args.push(arg_iden);

            if self.current_token.value == TokenVal::PuncComma {
                self.expect_and_eat_tok(TokenVal::PuncComma)?;
            } else if self.current_token.value != TokenVal::PuncCloseParen {
                return Err(self.new_syntax_error_at_current_token(
                    "Expected a ',' or ')' token after identifier in argument list.".to_string(),
                ));
            }
        }

        Ok(FnProto { args, iden })
    }

    fn parse_fn_declaration(&mut self) -> Result<Stmt, SyntaxError> {
        let proto = self.parse_fn_prototype()?;
        let body = Box::new(self.parse_block_statement()?);

        Ok(Stmt::new(StmtKind::Fn { proto, body }))
    }

    fn parse_block_statement(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::PuncOpenBrace)?;
        let mut statements: Vec<Stmt> = Vec::new();
        loop {
            if self.current_token.value == TokenVal::PuncCloseBrace {
                self.expect_and_eat_tok(TokenVal::PuncCloseBrace)?;
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(Stmt::new(StmtKind::Block { statements }))
    }
}
