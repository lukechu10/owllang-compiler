use clap::{App, Arg};
use owllang_lexer::Lexer;
use owllang_llvm_codegen::codegen_compilation_unit;
use owllang_parser::parser::Parser;
use std::io;
use std::io::prelude::*;

fn main() {
    let matches = App::new("OwlLang Compiler")
        .about("A compiler for the language that is not what it seems.")
        .version("0.1.0")
        .author("Luke Chu <lukechu10@users.noreply.github.com>")
        .arg(
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Shows the generated abstract syntax tree")
                .takes_value(false),
        )
        .get_matches();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        let mut input = String::new();
        print!("ready> ");
        stdout.flush().unwrap();

        let res = stdin.read_line(&mut input);
        match res {
            Ok(_) => {
                if input == ".quit\n" {
                    // exit repl
                    println!("Exiting repl.");
                    return;
                }

                let mut lexer = Lexer::with_string(input.as_str());
                // for token in &mut lexer {
                //     println!("{:?}", token);
                // }
                let mut parser = Parser::new(&mut lexer);
                let ast_result = parser.parse_compilation_unit();
                match ast_result {
                    Ok(ast) => {
                        if matches.is_present("show-ast") {
                            println!("{:#?}", ast);
                        }
                        match codegen_compilation_unit(&ast) {
                            Err(err) => println!("{:?}", err),
                            _ => {}
                        }
                    }
                    Err(err) => eprintln!(
                        "Error at {}({}:{}). Message = {}",
                        err.file_name, err.row, err.col, err.message
                    ),
                }
            }
            Err(err) => println!("Error: {}", err),
        }
    }
}
