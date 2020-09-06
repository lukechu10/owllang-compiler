use clap::{App, Arg, ArgMatches};
use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use owllang_lexer::Lexer;
use owllang_llvm_codegen::{c_str, codegen_compilation_unit, LlvmCodeGenVisitor};
use owllang_parser::parser::Parser;
use owllang_parser::Visitor;
use std::fs;
use std::io;
use std::io::prelude::*;

fn repl_loop(matches: &ArgMatches) {
    unsafe {
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        let context = LLVMGetGlobalContext();
        let mut module = LLVMModuleCreateWithNameInContext(c_str!("repl_tmp"), context);
        let builder = LLVMCreateBuilderInContext(context);

        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();

        let mut engine: LLVMExecutionEngineRef = std::ptr::null_mut();
        let error: *mut *mut i8 = std::ptr::null_mut();
        LLVMCreateExecutionEngineForModule(&mut engine, module, error);

        let target_machine = LLVMGetExecutionEngineTargetMachine(engine);
        let target_data_layout = LLVMCreateTargetDataLayout(target_machine);
        LLVMSetModuleDataLayout(module, target_data_layout);

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
                        break;
                    }

                    let mut lexer = Lexer::with_string(input.as_str());
                    let mut parser = Parser::new(&mut lexer);
                    let ast_result = parser.parse_compilation_unit();

                    match ast_result {
                        Ok(ast) => {
                            if matches.is_present("show-ast") {
                                println!("{:#?}", ast);
                            }
                            // match codegen_compilation_unit(&ast) {
                            //     Err(err) => println!("{:?}", err),
                            //     _ => {}
                            // }
                            let mut codegen_visitor = LlvmCodeGenVisitor::new(module, builder);
                            codegen_visitor.visit_compilation_unit(&ast).unwrap();
                            let last_func = LLVMGetLastFunction(module);
                            // LLVMDumpValue(last_func);
                            LLVMDumpModule(module);

                            LLVMAddModule(engine, module);
                            let mut args: Vec<LLVMGenericValueRef> = Vec::new();
                            let res = LLVMRunFunction(engine, last_func, 0, args.as_mut_ptr());
                            println!("Evaluated to: {}", LLVMGenericValueToInt(res, false as i32));
                            LLVMRemoveModule(engine, module, &mut module, error);
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

        LLVMDisposeModule(module);
        LLVMDisposeBuilder(builder);
        LLVMDisposeExecutionEngine(engine);
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
