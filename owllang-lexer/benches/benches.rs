use bencher::{benchmark_group, benchmark_main, Bencher};
use owllang_lexer::{Lexer, Token};
use owlc_error::ErrorReporter;

fn square_fn(bench: &mut Bencher) {
    let code = r#"
        fn square(x) {
            let result = x * x;
            return result;
        }
    "#;

    bench.iter(|| {
        let mut errors = ErrorReporter::new();
        let lexer = Lexer::with_string(code, &mut errors);
        let _tokens: Vec<Token> = lexer.collect();
    });
}

fn fibonacci_fn(bench: &mut Bencher) {
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

    bench.iter(|| {
        let mut errors = ErrorReporter::new();
        let lexer = Lexer::with_string(code, &mut errors);
        let _tokens: Vec<Token> = lexer.collect();
    });
}

benchmark_group!(benches, square_fn, fibonacci_fn);
benchmark_main!(benches);
