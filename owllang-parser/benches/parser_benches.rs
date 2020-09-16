use bencher::{benchmark_group, benchmark_main, Bencher};
use owlc_error::ErrorReporter;
use owlc_span::SourceFile;
use owllang_lexer::Lexer;
use owllang_parser::parser::Parser;
use std::rc::Rc;

fn parse_simple_fn(bench: &mut Bencher) {
    let code = r#"
        fn f() {
            return 0;
        }
    "#;
    let source_file = Rc::new(SourceFile::new("<tmp>", code));

    bench.iter(|| {
        let mut lex_errors = ErrorReporter::new(Rc::clone(&source_file));
        let mut lexer = Lexer::with_source_file(&source_file, &mut lex_errors);
        let mut parse_errors = ErrorReporter::new(Rc::clone(&source_file));
        let mut parser = Parser::new(&mut lexer, &mut parse_errors);
        parser.parse_compilation_unit();
    });
}

fn parse_square_fn(bench: &mut Bencher) {
    let code = r#"
        fn square(x) {
            let result = x * x;
            return result;
        }
    "#;
    let source_file = Rc::new(SourceFile::new("<tmp>", code));

    bench.iter(|| {
        let mut lex_errors = ErrorReporter::new(Rc::clone(&source_file));
        let mut lexer = Lexer::with_source_file(&source_file, &mut lex_errors);
        let mut parse_errors = ErrorReporter::new(Rc::clone(&source_file));
        let mut parser = Parser::new(&mut lexer, &mut parse_errors);
        parser.parse_compilation_unit();
    });
}

// fn parse_fibonacci_fn(bench: &mut Bencher) {
//     let code = r#"
//         fn fib(x) {
//             if x == 0 {
//                 return 0;
//             }
//             else {
//                 return fib(x - 1) + fib(x - 2);
//             }
//         }
//     "#;
//     let source_file = SourceFile::new("<tmp>", code);
//     let mut errors = ErrorReporter::new();
//     let lexer = Lexer::with_source_file(&source_file, &mut errors);
//     let _tokens: Vec<Token> = lexer.collect();

//     bench.iter(|| {});
// }

benchmark_group!(parser_benches, parse_simple_fn, parse_square_fn);
benchmark_main!(parser_benches);
