use crate::ast::{expressions::*, statements::*};
use crate::traits::*;
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
    /// User can input both fn definitions and statements in the repl prompt.
    pub fn parse_repl_input(&mut self) -> Result<Box<dyn StatementAST>, SyntaxError> {
        match self.current_token.value {
            TokenVal::KeywordFn => {
                let func = self.parse_fn_declaration()?;
                Ok(Box::new(func))
            }
            TokenVal::KeywordLet => {
                let let_statement = self.parse_let_statement()?;
                if self.current_token.value == TokenVal::PuncSemi {
                    self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                }
                Ok(Box::new(let_statement))
            }
            _ => {
                // try to parse expression statement
                let expression = self.parse_expression()?;
                // semi colon is optional in repl
                if self.current_token.value == TokenVal::PuncSemi {
                    self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                }
                let expr_statement = ExpressionStatementAST::new(expression);
                Ok(Box::new(expr_statement))
            }
        }
    }

    pub fn parse_compilation_unit(&mut self) -> Result<CompilationUnitAST, SyntaxError> {
        let func = self.parse_fn_declaration()?;
        Ok(CompilationUnitAST::new("entry".to_string(), vec![func]))
    }

    fn parse_statement(&mut self) -> Result<Box<dyn StatementAST>, SyntaxError> {
        match self.current_token.value {
            TokenVal::KeywordReturn => {
                let ret_statement = self.parse_return_statement()?;
                self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                Ok(Box::new(ret_statement))
            }
            TokenVal::KeywordLet => {
                let let_statement = self.parse_let_statement()?;
                self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                Ok(Box::new(let_statement))
            }
            _ => {
                // try to parse expression statement
                let expression = self.parse_expression()?;
                self.expect_and_eat_tok(TokenVal::PuncSemi)?;
                let expr_statement = ExpressionStatementAST::new(expression);
                Ok(Box::new(expr_statement))
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatementAST, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::KeywordReturn)?;
        let initializer = self.parse_expression()?;
        Ok(ReturnStatementAST::new(initializer))
    }

    fn parse_let_statement(&mut self) -> Result<LetStatementAST, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::KeywordLet)?;
        let identifier = self.expect_and_eat_iden_tok()?;
        self.expect_and_eat_tok(TokenVal::OpEquals)?;
        let initializer_value = self.parse_expression()?;
        Ok(LetStatementAST::new(identifier, initializer_value))
    }

    /// Parses any valid expression.
    fn parse_expression(&mut self) -> Result<Box<dyn ExprAST>, SyntaxError> {
        let lhs = self.parse_primary_expression()?;
        self.parse_binary_expr_rhs(lhs, OpPrecedence::Expression as i8)
    }

    /// Atomic expression.
    fn parse_primary_expression(&mut self) -> Result<Box<dyn ExprAST>, SyntaxError> {
        match self.current_token.value {
            TokenVal::LiteralInt(_) => {
                let literal_ast = self.parse_int_literal()?;
                Ok(Box::new(literal_ast))
            }
            TokenVal::Identifier(_) => self.parse_identifier_or_call_expr(),
            _ => Err(self.new_syntax_error_at_current_token(format!(
                "Unexpected {:?} token when expecting an expression.",
                self.current_token.value
            ))),
        }
    }

    fn parse_int_literal(&mut self) -> Result<LiteralExprAST<i32>, SyntaxError> {
        if let TokenVal::LiteralInt(num) = self.current_token.value {
            self.eat_token()?; // eat int literal token
            Ok(LiteralExprAST { value: num })
        } else {
            Err(self.new_syntax_error_at_current_token("Expected an int literal.".to_string()))
        }
    }

    /// Returns a `IdentifierExprAST` or `CallExprAST`, depending on the scenario.
    fn parse_identifier_or_call_expr(&mut self) -> Result<Box<dyn ExprAST>, SyntaxError> {
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
            let mut args: Vec<Box<dyn ExprAST>> = Vec::new();
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
            Ok(Box::new(CallExprAST::new(identifier, args)))
        } else {
            Ok(Box::new(IdentifierExprAST::new(identifier)))
        }
    }

    fn parse_binary_expr_rhs(
        &mut self,
        lhs: Box<dyn ExprAST>,
        prev_precedence: i8,
    ) -> Result<Box<dyn ExprAST>, SyntaxError> {
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

            lhs_tmp = Box::new(BinaryExprAST::new(lhs_tmp, rhs, binary_op_tok.value));
        }
    }

    fn parse_fn_prototype(&mut self) -> Result<PrototypeAST, SyntaxError> {
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

            if self.current_token.value == TokenVal::PuncCloseParen {
                self.expect_and_eat_tok(TokenVal::PuncCloseParen)?;
            } else if self.current_token.value != TokenVal::PuncComma {
                return Err(self.new_syntax_error_at_current_token(
                    "Expected a ',' or ')' token after identifier in argument list.".to_string(),
                ));
            }
        }

        Ok(PrototypeAST::new(iden, args))
    }

    fn parse_fn_declaration(&mut self) -> Result<FnStatementAST, SyntaxError> {
        let prototype = self.parse_fn_prototype()?;
        // unimplemented!()
        // Err(self.new_syntax_error_at_current_token("Not implemented!".to_string()))
        let body = self.parse_block_statement()?;

        Ok(FnStatementAST::new(prototype, body))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatementAST, SyntaxError> {
        self.expect_and_eat_tok(TokenVal::PuncOpenBrace)?;
        let mut statements: Vec<Box<dyn StatementAST>> = Vec::new();
        loop {
            if self.current_token.value == TokenVal::PuncCloseBrace {
                self.expect_and_eat_tok(TokenVal::PuncCloseBrace)?;
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(BlockStatementAST::new(statements))
    }
}