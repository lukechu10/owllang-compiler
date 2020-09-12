use owlc_error::ErrorReporter;
use owllang_lexer::Lexer;
use owllang_parser::parser::Parser;

#[test]
fn can_parse_function() {
    let str = "fn f() { return 0; }";
    let mut lexer_error_reporter = ErrorReporter::new();
    let mut error_reporter = ErrorReporter::new();
    let mut lexer = Lexer::with_string(str, &mut lexer_error_reporter);
    let mut parser = Parser::new(&mut lexer, &mut error_reporter);
    let ast = parser.parse_compilation_unit();

    error_reporter.merge_from(&mut lexer_error_reporter);

    assert!(!error_reporter.has_errors());
    assert_eq!(
        format!("{:?}", ast),
        r#"CompilationUnit { entry_file_name: "entry", functions: [Stmt { kind: Fn { proto: FnProto { args: [], iden: "f" }, body: Stmt { kind: Block { statements: [Stmt { kind: Return { value: Expr { kind: Literal(0) } } }] } } } }], errors: [] }"#
    );
}
