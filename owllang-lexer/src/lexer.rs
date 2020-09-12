use crate::{Token, TokenKind};
use owlc_error::{Error, ErrorReporter};
use std::iter::{Iterator, Peekable};
use std::str::Chars;

#[allow(dead_code)] // TODO remove allow
pub struct Lexer<'a> {
    /// The string to read from.
    input: &'a str,
    /// Should be 1 based because `row_num` is user facing.
    row_num: u32,
    /// Should be 1 based because `col_num` is user facing.
    col_num: u32,
    /// Iterator over all the characters in `input`.
    chars: Peekable<Chars<'a>>,

    errors: &'a mut ErrorReporter,
}

impl<'a> Lexer<'a> {
    pub fn with_string(input: &'a str, error_reporter: &'a mut ErrorReporter) -> Self {
        Lexer {
            input,
            row_num: 1,
            col_num: 0, // start at 0 because first call to next() will increment value
            chars: input.chars().peekable(),
            errors: error_reporter,
        }
    }

    /// Returns `true` if `c` is a valid character as the first character in an identifier. Else retrurns `false`.
    fn is_iden_start(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        }
    }

    /// Returns `true` if `c` is a valid character in the middle of an identifier. Else retrurns `false`.
    fn is_iden_continue(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
            _ => false,
        }
    }

    /// Util function to read next char. If char is a newline character, the line number is incremented.
    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next();

        let is_newline = c.is_some() && {
            let c_unwrapped = c.unwrap();
            // newline character
            c_unwrapped == '\n'
            // carriage return characters (\r\n)
                || (c_unwrapped == '\r'
                    && self.chars.peek().is_some()
                    && *self.chars.peek().unwrap() == '\n')
        };

        if is_newline {
            self.row_num += 1; // increment row number
            self.col_num = 1; // reset column number.
        } else {
            self.col_num += 1; // increment column number.
        }

        c
    }

    /// Utility function to peek next char without consuming it.
    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Utility factory function to create new tokens with current position.
    fn create_token(&self, value: TokenKind, len: u32) -> Token {
        Token {
            value,
            row: self.row_num,
            col: self.col_num + 1 - len, // self.col_num is position of end of token.
            len,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Returns the next token in the input or None if the end of the input is reached.
    fn next(&mut self) -> Option<Token> {
        let current_char = {
            // skip whitespace characters
            let mut tmp_char = self.next_char();

            while tmp_char.is_some() && tmp_char.unwrap().is_whitespace() {
                tmp_char = self.next_char(); // read next char
            }

            if let None = tmp_char {
                return None; // exit function
            } else {
                tmp_char.unwrap()
            }
        };

        match current_char {
            // punctuation
            '(' => Some(self.create_token(TokenKind::PuncOpenParen, 1)),
            ')' => Some(self.create_token(TokenKind::PuncCloseParen, 1)),
            '{' => Some(self.create_token(TokenKind::PuncOpenBrace, 1)),
            '}' => Some(self.create_token(TokenKind::PuncCloseBrace, 1)),
            ',' => Some(self.create_token(TokenKind::PuncComma, 1)),
            ';' => Some(self.create_token(TokenKind::PuncSemi, 1)),
            // operators
            '+' => Some(self.create_token(TokenKind::OpPlus, 1)),
            '-' => Some(self.create_token(TokenKind::OpMinus, 1)),
            '*' => Some(self.create_token(TokenKind::OpAsterisk, 1)),
            '/' => Some(self.create_token(TokenKind::OpSlash, 1)),
            '%' => Some(self.create_token(TokenKind::OpPercent, 1)),
            '=' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char(); // eat character
                        Some(self.create_token(TokenKind::OpEqualsEquals, 2))
                    }
                    _ => Some(self.create_token(TokenKind::OpEquals, 1)),
                }
            }
            '>' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char(); // eat character
                        Some(self.create_token(TokenKind::OpEqualsGreaterThan, 2))
                    }
                    _ => Some(self.create_token(TokenKind::OpGreaterThan, 1)),
                }
            }
            '<' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char(); // eat character
                        Some(self.create_token(TokenKind::OpEqualsLessThan, 2))
                    }
                    _ => Some(self.create_token(TokenKind::OpLessThan, 1)),
                }
            }
            '0'..='9' => {
                // lex int literal
                // TODO: lex double literal
                let mut int_str = current_char.to_string(); // add current_char to int_str
                let mut next_char = self.peek_char();

                while next_char.is_some() && next_char.unwrap().is_digit(10) {
                    int_str.push(self.next_char().unwrap());
                    next_char = self.peek_char();
                }

                let int: i64 = int_str.parse().unwrap();
                Some(self.create_token(TokenKind::LiteralInt(int), int_str.len() as u32))
            }
            c if Self::is_iden_start(c) => {
                // lex compound Token
                let mut iden_str = current_char.to_string();
                let mut next_char = self.peek_char();

                while next_char.is_some() && Self::is_iden_continue(*next_char.unwrap()) {
                    iden_str.push(self.next_char().unwrap());
                    next_char = self.peek_char();
                }

                let iden_str_len = iden_str.len() as u32;

                // check if iden_str is a keyword
                match iden_str {
                    s if s == "fn" => Some(self.create_token(TokenKind::KeywordFn, iden_str_len)),
                    s if s == "extern" => {
                        Some(self.create_token(TokenKind::KeywordExtern, iden_str_len))
                    }
                    s if s == "let" => Some(self.create_token(TokenKind::KeywordLet, iden_str_len)),
                    s if s == "if" => Some(self.create_token(TokenKind::KeywordIf, iden_str_len)),
                    s if s == "else" => {
                        Some(self.create_token(TokenKind::KeywordElse, iden_str_len))
                    }
                    s if s == "while" => {
                        Some(self.create_token(TokenKind::KeywordWhile, iden_str_len))
                    }
                    s if s == "for" => Some(self.create_token(TokenKind::KeywordFor, iden_str_len)),
                    s if s == "return" => {
                        Some(self.create_token(TokenKind::KeywordReturn, iden_str_len))
                    }
                    _ => Some(self.create_token(TokenKind::Identifier(iden_str), iden_str_len)),
                }
            }
            _ => {
                let error = Error {
                    file_name: "repl".to_string(), // FIXME,
                    row: self.row_num,
                    col: self.col_num,
                    message: format!("Unexpected {} character.", current_char),
                };
                self.errors.report(error);
                None
            }
        }
    }
}
