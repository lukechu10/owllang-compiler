use owlc_error::ErrorReporter;
use owlc_lexer::Lexer;
use owlc_parser::parser::Parser;
use owlc_parser::visitor::AstVisitor;
use owlc_passes::resolver::ResolverVisitor;
use owlc_span::SourceFile;
use std::rc::Rc;

#[test]
fn can_resolve_std_symbols() {
    let std_source = include_str!("../std.hoot");
    let source_file = Rc::new(SourceFile::new("<tmp>", std_source));
    let mut lex_errors = ErrorReporter::new(Rc::clone(&source_file));
    let mut lexer = Lexer::with_source_file(&source_file, &mut lex_errors);
    let mut parse_errors = ErrorReporter::new(Rc::clone(&source_file));
    let mut parser = Parser::new(&mut lexer, &mut parse_errors);
    let ast = parser.parse_compilation_unit();

    // resolve symbols
    let mut resolve_errors = ErrorReporter::new(Rc::clone(&source_file));
    let mut resolver_visitor = ResolverVisitor::new(&mut resolve_errors);

    resolver_visitor.visit_compilation_unit(&ast);

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
