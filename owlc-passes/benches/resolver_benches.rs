use bencher::{benchmark_group, benchmark_main, Bencher};
use owlc_error::ErrorReporter;
use owlc_passes::resolver::ResolverVisitor;
use owlc_span::SourceFile;
use owllang_lexer::Lexer;
use owllang_parser::parser::Parser;
use owllang_parser::visitor::AstVisitor;

fn resolve_std_symbols(bench: &mut Bencher) {
    let std_source = include_str!("../std.hoot");
    let source_file = SourceFile::new("<tmp", std_source);
    let mut lex_errors = ErrorReporter::new();
    let mut lexer = Lexer::with_source_file(&source_file, &mut lex_errors);
    let mut parse_errors = ErrorReporter::new();
    let mut parser = Parser::new(&mut lexer, &mut parse_errors);
    let ast = parser.parse_compilation_unit();

    bench.iter(|| {
        // resolve symbols
        let mut resolve_errors = ErrorReporter::new();
        let mut resolver_visitor = ResolverVisitor::new(&mut resolve_errors);

        resolver_visitor.visit_compilation_unit(&ast);
    })
}

benchmark_group!(resolver_benches, resolve_std_symbols);
benchmark_main!(resolver_benches);
