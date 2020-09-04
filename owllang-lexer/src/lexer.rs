use crate::{Token, TokenVal};
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
}

impl<'a> Lexer<'a> {
    pub fn with_string(input: &'a str) -> Self {
        Lexer {
            input,
            row_num: 1,
            col_num: 0, // start at 0 because first call to next() will increment value
            chars: input.chars().peekable(),
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
    fn create_token(&self, value: TokenVal, len: u32) -> Token {
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
            '(' => Some(self.create_token(TokenVal::PuncOpenParen, 1)),
            ')' => Some(self.create_token(TokenVal::PuncCloseParen, 1)),
            '{' => Some(self.create_token(TokenVal::PuncOpenBrace, 1)),
            '}' => Some(self.create_token(TokenVal::PuncCloseBrace, 1)),
            ',' => Some(self.create_token(TokenVal::PuncComma, 1)),
            ';' => Some(self.create_token(TokenVal::PuncSemi, 1)),
            // operators
            '+' => Some(self.create_token(TokenVal::OpPlus, 1)),
            '-' => Some(self.create_token(TokenVal::OpMinus, 1)),
            '*' => Some(self.create_token(TokenVal::OpAsterix, 1)),
            '/' => Some(self.create_token(TokenVal::OpSlash, 1)),
            '%' => Some(self.create_token(TokenVal::OpPercent, 1)),
            '=' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char(); // eat character
                        Some(self.create_token(TokenVal::OpEqualsEquals, 2))
                    }
                    _ => Some(self.create_token(TokenVal::OpEquals, 1)),
                }
            }
            '>' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char(); // eat character
                        Some(self.create_token(TokenVal::OpEqualsGreaterThan, 2))
                    }
                    _ => Some(self.create_token(TokenVal::OpGreaterThan, 1)),
                }
            }
            '<' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char(); // eat character
                        Some(self.create_token(TokenVal::OpEqualsLessThan, 2))
                    }
                    _ => Some(self.create_token(TokenVal::OpLessThan, 1)),
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
                Some(self.create_token(TokenVal::LiteralInt(int), int_str.len() as u32))
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
                    s if s == "fn" => Some(self.create_token(TokenVal::KeywordFn, iden_str_len)),
                    s if s == "extern" => {
                        Some(self.create_token(TokenVal::KeywordExtern, iden_str_len))
                    }
                    s if s == "let" => Some(self.create_token(TokenVal::KeywordLet, iden_str_len)),
                    s if s == "if" => Some(self.create_token(TokenVal::KeywordIf, iden_str_len)),
                    s if s == "else" => {
                        Some(self.create_token(TokenVal::KeywordElse, iden_str_len))
                    }
                    s if s == "while" => {
                        Some(self.create_token(TokenVal::KeywordWhile, iden_str_len))
                    }
                    s if s == "for" => Some(self.create_token(TokenVal::KeywordFor, iden_str_len)),
                    s if s == "return" => Some(self.create_token(TokenVal::KeywordReturn, iden_str_len)),
                    _ => Some(self.create_token(TokenVal::Identifier(iden_str), iden_str_len)),
                }
            }
            _ => panic!(format!("Unexpected character {}", current_char)),
        }
    }
}
