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

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
#[repr(i8)]
pub enum OpPrecedence {
    /// `OpPrecedence` for non operator `TokenVal`s.
    Error = -1,
    /// Used to except all operator.
    Expression = 0,
    // `=`
    Assignment,
    // `==`
    Equality,
    // `>`, `>=`, `<` and `<=`
    Relational,
    /// `+`, and `-`
    Additive,
    /// `*`, `/` and `%`
    Multiplicative,
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
    /// Returns the precedence of the current `TokenVal`. If the current `TokenVal` is not an operator, returns the `Error` variant of the `OPPrecedence` enum.
    /// # Panics
    /// This method cannot panic.
    pub fn precedence(&self) -> OpPrecedence {
        match self {
            TokenKind::OpEquals => OpPrecedence::Assignment,
            TokenKind::OpEqualsEquals => OpPrecedence::Equality,
            TokenKind::OpGreaterThan
            | TokenKind::OpEqualsGreaterThan
            | TokenKind::OpLessThan
            | TokenKind::OpEqualsLessThan => OpPrecedence::Relational,
            TokenKind::OpPlus | TokenKind::OpMinus => OpPrecedence::Additive,
            TokenKind::OpAsterisk | TokenKind::OpSlash | TokenKind::OpPercent => {
                OpPrecedence::Multiplicative
            }
            _ => OpPrecedence::Error,
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

        return str.to_string();
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_name())
    }
}