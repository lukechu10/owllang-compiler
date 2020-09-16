use owlc_error::ErrorReporter;
use owlc_span::SourceFile;
use owllang_lexer::{Lexer, TokenKind};
use std::rc::Rc;

#[test]
fn lex_fn_definition() {
    let code = r#"
        fn square(x) {
            let a = x * x;
            return a;
        }
    "#;
    let source_file = Rc::new(SourceFile::new("<tmp>", code));
    let mut errors = ErrorReporter::new(Rc::clone(&source_file));
    let lexer = Lexer::with_source_file(&source_file, &mut errors);

    let tokens: Vec<TokenKind> = lexer.map(|token| token.kind).collect();
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
