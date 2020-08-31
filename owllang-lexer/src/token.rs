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
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordFor,
    // identifiers
    Identifier(String),
    // literals
    LiteralInt(i32),
    LiteralDouble(f32),
}
