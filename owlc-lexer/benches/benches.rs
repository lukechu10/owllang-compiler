use bencher::{benchmark_group, benchmark_main, Bencher};
use owlc_lexer::{Lexer, Token};
use owlc_error::ErrorReporter;
use owlc_span::SourceFile;
use std::rc::Rc;

fn lex_square_fn(bench: &mut Bencher) {
    let code = r#"
        fn square(x) {
            let result = x * x;
            return result;
        }
    "#;
    let source_file = Rc::new(SourceFile::new("<tmp>", code));

    bench.iter(|| {
        let mut errors = ErrorReporter::new(Rc::clone(&source_file));
        let lexer = Lexer::with_source_file(&source_file, &mut errors);
        let _tokens: Vec<Token> = lexer.collect();
    });
}

fn lex_fibonacci_fn(bench: &mut Bencher) {
    let code = r#"
        fn fib(x) {
            if x == 0 {
                return 0;
            }
            else {
                return fib(x - 1) + fib(x - 2);
            }
        }
    "#;
    let source_file = Rc::new(SourceFile::new("<tmp>", code));

    bench.iter(|| {
        let mut errors = ErrorReporter::new(Rc::clone(&source_file));
        let lexer = Lexer::with_source_file(&source_file, &mut errors);
        let _tokens: Vec<Token> = lexer.collect();
    });
}

benchmark_group!(benches, lex_square_fn, lex_fibonacci_fn);
benchmark_main!(benches);
