use owlc_error::ErrorReporter;
use owlc_passes::resolver::ResolverVisitor;
use owlc_span::SourceFile;
use owllang_lexer::Lexer;
use owllang_parser::parser::Parser;
use owllang_parser::Visitor;

#[test]
fn can_resolve_std_symbols() {
    let std_source = include_str!("../std.hoot");
    let source_file = SourceFile::new("<tmp", std_source);
    let mut lex_errors = ErrorReporter::new();
    let mut lexer = Lexer::with_source_file(&source_file, &mut lex_errors);
    let mut parse_errors = ErrorReporter::new();
    let mut parser = Parser::new(&mut lexer, &mut parse_errors);
    let ast = parser.parse_compilation_unit();

    // resolve symbols
    let mut resolve_errors = ErrorReporter::new();
    let mut resolver_visitor = ResolverVisitor::new(&mut resolve_errors);

    resolver_visitor.visit_compilation_unit(&ast).unwrap();

    assert!(resolver_visitor
        .symbols
        .lookup(&"println".to_string())
        .is_some());
    assert!(resolver_visitor
        .symbols
        .lookup(&"read_num".to_string())
        .is_some());
    assert!(resolver_visitor
        .symbols
        .lookup(&"not_a_func".to_string())
        .is_none());
    assert!(
        resolver_visitor.symbols.lookup(&"".to_string()).is_none(),
        "Function name cannot be empty string."
    );
}
