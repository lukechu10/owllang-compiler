use owllang_parser::parser::Parser;
use owllang_lexer::Lexer;

#[test]
fn can_parse_function() {
    let str = "fn f() { return 0; }";
    let mut lexer = Lexer::with_string(str);
    let mut parser = Parser::new(&mut lexer);

    let ast = parser.parse_compilation_unit().unwrap();
    assert_eq!(format!("{:?}", ast), r#"CompilationUnitAST { entry_file_name: "entry", functions: [FnStatementAST { prototype: PrototypeAST { arguments: [], fn_identifier: "f" }, is_extern: false, body: Some(BlockStatementAST { statements: [ReturnStatementAST { ret_value: Expr { kind: Literal(0) } }] }) }] }"#);
}
