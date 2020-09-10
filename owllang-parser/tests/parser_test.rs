use owllang_parser::parser::Parser;
use owllang_lexer::Lexer;
use owlc_error::ErrorReporter;

#[test]
fn can_parse_function() {
    let str = "fn f() { return 0; }";
    let mut lexer = Lexer::with_string(str);
    let error_reporter = ErrorReporter::new();
    let mut parser = Parser::new(&mut lexer, error_reporter);

    let ast = parser.parse_compilation_unit();
    assert_eq!(format!("{:?}", ast), r#"CompilationUnit { entry_file_name: "entry", functions: [Stmt { kind: Fn { proto: FnProto { args: [], iden: "f" }, body: Stmt { kind: Block { statements: [Stmt { kind: Return { value: Expr { kind: Literal(0) } } }] } } } }], errors: [] }"#);
}
