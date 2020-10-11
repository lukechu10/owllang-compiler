use bencher::{benchmark_group, benchmark_main, Bencher};
use owlc_error::ErrorReporter;
use owlc_passes::resolver::ResolverVisitor;
use owlc_span::SourceFile;
use owlc_lexer::Lexer;
use owlc_parser::parser::Parser;
use owlc_parser::visitor::AstVisitor;
use std::rc::Rc;

fn resolve_std_symbols(bench: &mut Bencher) {
    let std_source = include_str!("../std.hoot");
    let source_file = Rc::new(SourceFile::new("<tmp", std_source));
    let mut lex_errors = ErrorReporter::new(Rc::clone(&source_file));
    let mut lexer = Lexer::with_source_file(&source_file, &mut lex_errors);
    let mut parse_errors = ErrorReporter::new(Rc::clone(&source_file));
    let mut parser = Parser::new(&mut lexer, &mut parse_errors);
    let ast = parser.parse_compilation_unit();

    bench.iter(|| {
        // resolve symbols
        let mut resolve_errors = ErrorReporter::new(Rc::clone(&source_file));
        let mut resolver_visitor = ResolverVisitor::new(&mut resolve_errors);

        resolver_visitor.visit_compilation_unit(&ast);
    })
}

benchmark_group!(resolver_benches, resolve_std_symbols);
benchmark_main!(resolver_benches);
