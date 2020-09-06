use clap::{App, Arg, ArgMatches};
use owllang_lexer::Lexer;
use owllang_llvm_codegen::codegen_compilation_unit;
use owllang_parser::parser::Parser;
use std::fs;
use std::io;
use std::io::prelude::*;

fn repl_loop(matches: &ArgMatches) {
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

/// Generated LLVM IR for the specified file.
/// # Panics
/// This method panics if `matches.value_of("input")` is `None`.
/// This method panics if the path does not exist. // TODO
/// This method panics if `parser.parse_compilation_unit()` returns an `Err`. // TODO
fn compile_file(matches: ArgMatches) {
    let path = matches.value_of("input").unwrap();
    let file_str = fs::read_to_string(path).unwrap();

    let mut lexer = Lexer::with_string(file_str.as_str());
    let mut parser = Parser::new(&mut lexer);

    let ast = parser.parse_compilation_unit().unwrap();
    codegen_compilation_unit(&ast).unwrap();
}

fn main() {
    let matches = App::new("OwlLang Compiler")
        .about("A compiler for the language that is not what it seems.")
        .version("0.1.0")
        .author("Luke Chu <lukechu10@users.noreply.github.com>")
        .arg(
            Arg::with_name("input")
                .index(1)
                .required(false)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Shows the generated abstract syntax tree")
                .takes_value(false),
        )
        .get_matches();

    match matches.value_of("input") {
        Some(_) => compile_file(matches),
        None => repl_loop(&matches),
    }
}
