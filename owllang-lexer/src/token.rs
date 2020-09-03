/// Represents a single token.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The value of the token.
    pub value: TokenVal,
    /// The row position of the token in the file / string.
    pub row: u32,
    /// The col position of the token in the file / string.
    pub col: u32,
    /// The length of the token.
    pub len: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
#[repr(i8)]
pub enum OpPrecedence {
    /// `OpPrecedence` for non operator `TokenVal`s.
    Error = -1,
    /// Used to except all operator.
    Expression = 0,
    Assignment,
    Equality,
    Relational,
    /// `+`, and `-`
    Additive,
    /// `*`, `/` and `%`
    Multiplicative,
}

/// Represents the value of a token.
#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum TokenVal {
    // punctuation
    PuncOpenParen,  // '('
    PuncCloseParen, // ')'
    PuncOpenBrace,  // '{'
    PuncCloseBrace, // '}'
    PuncComma,      // ','
    PuncSemi,       // ;'
    // operators
    OpPlus,    // '+'
    OpMinus,   // '-'
    OpAsterix, // '*'
    OpSlash,   // '/'
    OpPercent, // '%' (for modulo operator)
    OpEquals,  // '='

    OpEqualsEquals,      // '=='
    OpGreaterThan,       // '>'
    OpEqualsGreaterThan, // '>='
    OpLessThan,          // '<'
    OpEqualsLessThan,    // '<='
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
    LiteralInt(i32),
    LiteralDouble(f32),
    // miscaleneous
    EndOfFile,
}

impl TokenVal {
    /// Returns the precedence of the current `TokenVal`. If the current `TokenVal` is not an operator, returns the `Error` variant of the `OPPrecedence` enum.
    /// # Panics
    /// This method cannot panic.
    pub fn precedence(&self) -> OpPrecedence {
        match self {
            TokenVal::OpEquals => OpPrecedence::Assignment,
            TokenVal::OpEqualsEquals => OpPrecedence::Equality,
            TokenVal::OpGreaterThan
            | TokenVal::OpEqualsGreaterThan
            | TokenVal::OpLessThan
            | TokenVal::OpEqualsLessThan => OpPrecedence::Relational,
            TokenVal::OpPlus | TokenVal::OpMinus => OpPrecedence::Additive,
            TokenVal::OpAsterix | TokenVal::OpSlash | TokenVal::OpPercent => {
                OpPrecedence::Multiplicative
            }
            _ => OpPrecedence::Error,
        }
    }
}
