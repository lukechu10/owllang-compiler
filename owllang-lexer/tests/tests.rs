use owlc_error::ErrorReporter;
use owllang_lexer::{Lexer, TokenKind};

#[test]
fn lex_fn_definition() {
    let mut errors = ErrorReporter::new();
    let lexer = Lexer::with_string(
        r#"
        fn square(x) {
            let a = x * x;
            return a;
        }
    "#,
        &mut errors,
    );

    let tokens: Vec<TokenKind> = lexer.map(|token| token.value).collect();
    assert!(!errors.has_errors());
    assert_eq!(
        tokens,
        vec![
            TokenKind::KeywordFn,
            TokenKind::Identifier("square".to_string()),
            TokenKind::PuncOpenParen,
            TokenKind::Identifier("x".to_string()),
            TokenKind::PuncCloseParen,
            TokenKind::PuncOpenBrace,
            TokenKind::KeywordLet,
            TokenKind::Identifier("a".to_string()),
            TokenKind::OpEquals,
            TokenKind::Identifier("x".to_string()),
            TokenKind::OpAsterisk,
            TokenKind::Identifier("x".to_string()),
            TokenKind::PuncSemi,
            TokenKind::KeywordReturn,
            TokenKind::Identifier("a".to_string()),
            TokenKind::PuncSemi,
            TokenKind::PuncCloseBrace,
        ]
    );
}
