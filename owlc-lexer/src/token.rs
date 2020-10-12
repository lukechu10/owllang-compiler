use owlc_span::Span;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Represents a single token.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The internal data of the token.
    pub kind: TokenKind,
    /// The position of the `Token` in the `SourceFile`.
    pub loc: Span,
}

/// Represents the value of a token.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
    // punctuation
    /// `(`
    PuncOpenParen,
    /// `)`
    PuncCloseParen,
    /// `{`
    PuncOpenBrace,
    /// `}`
    PuncCloseBrace,
    /// `,`
    PuncComma,
    /// `;`
    PuncSemi,
    // operators
    /// `+`
    OpPlus,
    /// `-`
    OpMinus,
    /// `*`
    OpAsterisk,
    /// `/`
    OpSlash,
    /// `%` (for modulo operator)
    OpPercent,
    /// `=`
    OpEquals,

    /// `==`
    OpEqualsEquals,
    /// `>`
    OpGreaterThan,
    /// `>=`
    OpEqualsGreaterThan,
    /// `<`
    OpLessThan,
    /// `<=`
    OpEqualsLessThan,
    /// `.` (for function composition)
    OpDot,

    // keywords
    KeywordFn,
    KeywordExtern,
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordFor,
    KeywordReturn,
    // identifiers
    Identifier(String),
    // literals
    LiteralInt(i64),
    /// **Note**: Current unused.
    LiteralDouble(f32),
    // miscellaneous
    EndOfFile,
}

impl TokenKind {
    /// Returns the infix binding power for an operator. If invalid operator, returns `(-1, -1)`.
    /// Refer to [https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) for details on Pratt Parsing algorithm.
    /// # Panics
    /// This method cannot panic.
    pub fn infix_binding_power(&self) -> (i8, i8) {
        match self {
            TokenKind::OpEquals => (1, 2),
            TokenKind::OpEqualsEquals => (3, 4),
            TokenKind::OpGreaterThan
            | TokenKind::OpEqualsGreaterThan
            | TokenKind::OpLessThan
            | TokenKind::OpEqualsLessThan => (5, 6),
            TokenKind::OpPlus | TokenKind::OpMinus => (7, 8),
            TokenKind::OpAsterisk | TokenKind::OpSlash | TokenKind::OpPercent => (9, 10),
            TokenKind::OpDot => (14, 13), // right associative
            _ => (-1, -1),
        }
    }

    /// Returns the prefix binding power for an operator. If invalid operator, returns `((), -1)`.
    /// Refer to [https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) for details on Pratt Parsing algorithm.
    /// # Panics
    /// This method cannot panic.
    pub fn prefix_binding_power(&self) -> ((), i8) {
        match self {
            TokenKind::OpPlus | TokenKind::OpMinus => ((), 11),
            _ => ((), -1),
        }
    }

    /// Returns the display name of the `TokenKind`.
    pub fn token_name(&self) -> String {
        let str = match self {
            TokenKind::PuncOpenParen => "'('",
            TokenKind::PuncCloseParen => "')'",
            TokenKind::PuncOpenBrace => "'{'",
            TokenKind::PuncCloseBrace => "'}'",
            TokenKind::PuncComma => "','",
            TokenKind::PuncSemi => "';'",
            TokenKind::OpPlus => "'+'",
            TokenKind::OpMinus => "'-'",
            TokenKind::OpAsterisk => "'*'",
            TokenKind::OpSlash => "'/'",
            TokenKind::OpPercent => "'%'",
            TokenKind::OpEquals => "'='",
            TokenKind::OpEqualsEquals => "'=='",
            TokenKind::OpGreaterThan => "'>'",
            TokenKind::OpEqualsGreaterThan => "'>='",
            TokenKind::OpLessThan => "'<'",
            TokenKind::OpEqualsLessThan => "'<='",
            TokenKind::OpDot => "'.'",
            TokenKind::KeywordFn => "'fn'",
            TokenKind::KeywordExtern => "'extern'",
            TokenKind::KeywordLet => "'let'",
            TokenKind::KeywordIf => "'if'",
            TokenKind::KeywordElse => "'else'",
            TokenKind::KeywordWhile => "'while'",
            TokenKind::KeywordFor => "'for'",
            TokenKind::KeywordReturn => "'return'",
            TokenKind::Identifier(_) => "identifier",
            TokenKind::LiteralInt(_) => "integer literal",
            TokenKind::LiteralDouble(_) => "double literal",
            TokenKind::EndOfFile => "end of file",
        };

        str.to_string()
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_name())
    }
}
