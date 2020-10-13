use crate::{Token, TokenKind};
use owlc_error::{Error, ErrorReporter};
use owlc_span::{BytePos, SourceFile};
use std::iter::{Iterator, Peekable};
use std::rc::Rc;
use std::str::CharIndices;

pub struct Lexer<'a> {
    /// The string to read from.
    pub src: Rc<SourceFile>,
    /// Iterator over all the characters in `input`.
    char_indices: Peekable<CharIndices<'a>>,

    /// The byte pos of the current char being read. Should be updated when `self.chars_indices.next()` is called.
    current_byte_pos: u32,

    errors: &'a mut ErrorReporter,
}

impl<'a> Lexer<'a> {
    pub fn with_source_file(
        source_file: &'a Rc<SourceFile>,
        error_reporter: &'a mut ErrorReporter,
    ) -> Self {
        Lexer {
            src: Rc::clone(source_file),
            char_indices: source_file.src.char_indices().peekable(),
            current_byte_pos: 0,
            errors: error_reporter,
        }
    }

    /// Returns `true` if `c` is a valid character as the first character in an identifier. Else returns `false`.
    fn is_iden_start(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        }
    }

    /// Returns `true` if `c` is a valid character in the middle of an identifier. Else returns `false`.
    fn is_iden_continue(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
            _ => false,
        }
    }

    /// Util function to read next char.
    fn next_char(&mut self) -> Option<char> {
        match self.char_indices.next() {
            Some(x) => {
                self.current_byte_pos = x.0 as u32;
                Some(x.1)
            }
            None => None,
        }
    }

    /// Utility function to peek next char without consuming it.
    fn peek_char(&mut self) -> Option<char> {
        match self.char_indices.peek() {
            Some(x) => Some(x.1),
            None => None,
        }
    }

    /// Utility factory function to create new tokens with current position.
    fn create_token(&self, value: TokenKind, len: u32) -> Token {
        let span = BytePos(self.current_byte_pos + 1 - len).to(BytePos(self.current_byte_pos));
        Token {
            kind: value,
            loc: span,
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

            tmp_char?
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
            '/' => {
                match self.peek_char() {
                    Some('/') => {
                        // line comment, read to end of line and discard
                        let mut tmp_char = self.next_char();
                        while !(tmp_char == None || tmp_char.unwrap() == '\n') {
                            tmp_char = self.next_char();
                        }
                        debug_assert!(self.peek_char() != Some('\n')); // next char should not be '\n'
                        self.next() // recursively read next token
                    }
                    _ => Some(self.create_token(TokenKind::OpSlash, 1)),
                }
            }
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
            '.' => Some(self.create_token(TokenKind::OpDot, 1)),
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
                let error = Error::new(
                    self.src.name.to_string(),
                    BytePos(self.current_byte_pos).to(BytePos(self.current_byte_pos)),
                    format!("Unexpected {} character.", current_char),
                );
                self.errors.report(error);
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    /// Utility function
    fn lex_str(s: &str) -> Vec<Token> {
        let source = Rc::new(SourceFile::new("<test>", s));
        let mut error_reporter = ErrorReporter::new(source.clone());

        let lexer = Lexer::with_source_file(&source, &mut error_reporter);
        let tokens = lexer.collect();
        
        assert!(!error_reporter.has_errors());
        tokens
    }

    #[test]
    fn punctuation() {
        assert_debug_snapshot!(lex_str("+ - * / % . ( ) { } , ;"));
        assert_debug_snapshot!(lex_str("== <= >= < >"));
    }

    #[test]
    fn keywords() {
        assert_debug_snapshot!(lex_str("fn extern let if else while for return"));
    }

    #[test]
    fn int_literal() {
        assert_debug_snapshot!(lex_str("1"));
        assert_debug_snapshot!(lex_str("1120323489"));
    }

    #[test]
    fn identifier() {
        assert_debug_snapshot!(lex_str("my_variable"));
        assert_debug_snapshot!(lex_str("_my_variable")); // can start with '_'
    }

    #[test]
    #[should_panic]
    fn bad_tok() {
        lex_str("?"); // invalid char
    }
}