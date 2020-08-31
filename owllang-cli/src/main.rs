fn main() {
    use owllang_lexer::Lexer;
    use std::io;
    use std::io::prelude::*;

    // let lexer = Lexer::with_string("fn test() { let x = 1; x }");
    // for tok in lexer {
    //     println!("{:?}", tok);
    // }

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        let mut input = String::new();
        print!("ready> ");
        stdout.flush().unwrap();

        let res = stdin.read_line(&mut input);
        match res {
            Ok(_) => {
                let lexer = Lexer::with_string(input.as_str());
                for token in lexer {
                    println!("{:?}", token);
                }
            }
            Err(err) => println!("Error: {}", err),
        }
    }
}
