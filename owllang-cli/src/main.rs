fn main() {
    use owllang_lexer::Lexer;
    use owllang_parser::parser::Parser;
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
                let mut lexer = Lexer::with_string(input.as_str());
                // for token in &mut lexer {
                //     println!("{:?}", token);
                // }
                let mut parser = Parser::new(&mut lexer);
                let ast_result = parser.parse_compilation_unit();
                match ast_result {
                    Ok(ast) => println!("{:?}", ast),
                    Err(err) => eprintln!("Error at {}({}:{}). Message = {}", err.file_name, err.row, err.col, err.message),
                }
            }
            Err(err) => println!("Error: {}", err),
        }
    }
}
