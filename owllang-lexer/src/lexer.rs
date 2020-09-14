use crate::{Token, TokenKind};
use owlc_error::{Error, ErrorReporter};
use owlc_span::{BytePos, SourceFile};
use std::iter::{Iterator, Peekable};
use std::str::{CharIndices, Chars};

pub struct Lexer<'a> {
    /// The string to read from.
    pub src: &'a SourceFile,
    /// Should be 1 based because `row_num` is user facing.
    row_num: u32,
    /// Should be 1 based because `col_num` is user facing.
    col_num: u32,
    /// Iterator over all the characters in `input`.
    chars: Peekable<Chars<'a>>,
    chars_indices: Peekable<CharIndices<'a>>,

    /// The byte pos of the current char being read. Should be updated when `self.chars_indices.next()` is called.
    current_byte_pos: u32,

    errors: &'a mut ErrorReporter,
}

impl<'a> Lexer<'a> {
    pub fn with_source_file(
        source_file: &'a SourceFile,
        error_reporter: &'a mut ErrorReporter,
    ) -> Self {
        let chars = source_file.src.chars().peekable();
        let chars_indices = source_file.src.char_indices().peekable();
        Lexer {
            src: source_file,
            row_num: 1,
            col_num: 0, // start at 0 because first call to next() will increment value
            chars,
            chars_indices,
            current_byte_pos: 0,
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

    /// Util function to read next char.
    fn next_char(&mut self) -> Option<char> {
        match self.chars_indices.next() {
            Some(x) => {
                self.current_byte_pos = x.0 as u32;
                Some(x.1)
            }
            None => None,
        }
    }

    /// Utility function to peek next char without consuming it.
    fn peek_char(&mut self) -> Option<char> {
        match self.chars_indices.peek() {
            Some(x) => Some(x.1),
            None => None,
        }
    }

    /// Utility factory function to create new tokens with current position.
    fn create_token(&self, value: TokenKind, len: u32) -> Token {
        let span = BytePos(self.current_byte_pos + 1 - len).to(BytePos(self.current_byte_pos));
        Token { value, loc: span }
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

                while next_char.is_some() && Self::is_iden_continue(next_char.unwrap()) {
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
                    loc: BytePos(self.current_byte_pos).to(BytePos(self.current_byte_pos)),
                    message: format!("Unexpected {} character.", current_char),
                };
                self.errors.report(error);
                None
            }
        }
    }
}
