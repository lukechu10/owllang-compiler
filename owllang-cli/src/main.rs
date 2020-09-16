use clap::{App, Arg, ArgMatches};
use llvm_sys::support::*;
use llvm_sys::{
    analysis::*, bit_writer::*, core::*, execution_engine::*, target::*, target_machine::*,
    transforms::util::*,
};
use owlc_error::ErrorReporter;
use owlc_passes::resolver::ResolverVisitor;
use owlc_span::SourceFile;
use owllang_lexer::Lexer;
use owllang_llvm_codegen::{c_str, LlvmCodeGenVisitor};
use owllang_parser::{ast::statements::StmtKind, parser::Parser, visitor::AstVisitor};
use std::rc::Rc;
use std::{fs, io, io::prelude::*};

#[no_mangle]
pub extern "C" fn println(num: i64) -> i64 {
    println!("{}", num);
    num
}

#[no_mangle]
pub extern "C" fn read_num() -> i64 {
    let stdin = io::stdin();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    let num: i64 = str::parse(&line.trim()).unwrap();
    num
}

fn repl_loop(matches: &ArgMatches) {
    unsafe {
        LLVMAddSymbol(c_str!("println"), println as *mut std::ffi::c_void);
        LLVMAddSymbol(c_str!("read_num"), read_num as *mut std::ffi::c_void);

        let stdin = io::stdin();
        let mut stdout = io::stdout();

        let context = LLVMGetGlobalContext();
        let mut module = LLVMModuleCreateWithNameInContext(c_str!("repl"), context);
        let builder = LLVMCreateBuilderInContext(context);
        let mut codegen_visitor = LlvmCodeGenVisitor::new(module, builder);
        // codegen_visitor.add_builtin_fns();

        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();

        let mut engine: LLVMExecutionEngineRef = std::ptr::null_mut();
        let error: *mut *mut i8 = std::ptr::null_mut();
        LLVMCreateExecutionEngineForModule(&mut engine, module, error);

        let target_machine = LLVMGetExecutionEngineTargetMachine(engine);
        let target_data_layout = LLVMCreateTargetDataLayout(target_machine);
        LLVMSetModuleDataLayout(module, target_data_layout);

        let mut symbol_table;

        // codegen std.hoot
        let std_hoot = include_str!("../../owlc-passes/std.hoot");
        let std_source_file = Rc::new(SourceFile::new("std.hoot", std_hoot));

        let mut std_error_reporter = ErrorReporter::new(Rc::clone(&std_source_file));
        let mut std_lexer = Lexer::with_source_file(&std_source_file, &mut std_error_reporter);

        let mut parser_error_reporter = ErrorReporter::new(Rc::clone(&std_source_file));
        let mut std_parser = Parser::new(&mut std_lexer, &mut parser_error_reporter);
        let std_ast = std_parser.parse_compilation_unit();

        let mut resolver_visitor = ResolverVisitor::new(&mut std_error_reporter);
        resolver_visitor.visit_compilation_unit(&std_ast);
        symbol_table = resolver_visitor.symbols;

        // merge error reporter
        std_error_reporter.merge_from(&mut parser_error_reporter);
        if std_error_reporter.has_errors() {
            panic!("Errors in standard library. Aborting.");
        } else {
            codegen_visitor.visit_compilation_unit(&std_ast);
        }

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

                    let source_file = Rc::new(SourceFile::new("<repl>", input.as_str()));
                    let mut error_reporter = ErrorReporter::new(Rc::clone(&source_file));
                    let mut lexer_error_reporter = ErrorReporter::new(Rc::clone(&source_file));
                    let mut lexer =
                        Lexer::with_source_file(&source_file, &mut lexer_error_reporter);

                    let ast = {
                        let mut parser = Parser::new(&mut lexer, &mut error_reporter);
                        let stmt = parser.parse_repl_input();

                        let mut resolver_visitor = ResolverVisitor::new(&mut error_reporter);
                        // restore previous symbols
                        resolver_visitor.symbols = symbol_table;

                        resolver_visitor.visit_stmt(&stmt);

                        // add resolved symbols to symbol_table
                        symbol_table = resolver_visitor.symbols;
                        stmt
                    };

                    error_reporter.merge_from(&mut lexer_error_reporter);

                    if error_reporter.has_errors() {
                        print!("{}", error_reporter);
                        continue; // do not evaluate result
                    }

                    if matches.is_present("show-ast") {
                        println!("{:#?}", ast);
                    }

                    // True if repl should evaluate input.
                    let evaluate_res = match ast.kind {
                        StmtKind::ExprSemi { expr: _ } => true,
                        _ => false,
                    };

                    codegen_visitor.handle_repl_input(ast);

                    let last_func = LLVMGetLastFunction(module);
                    LLVMVerifyFunction(
                        last_func,
                        LLVMVerifierFailureAction::LLVMPrintMessageAction,
                    );
                    // optimization passes
                    let pm = LLVMCreatePassManager();
                    LLVMAddPromoteMemoryToRegisterPass(pm);
                    LLVMRunPassManager(pm, module);
                    LLVMDisposePassManager(pm);

                    if matches.is_present("dump-llvm") {
                        LLVMDumpValue(last_func);
                    }

                    if evaluate_res {
                        LLVMAddModule(engine, module);
                        let mut args: Vec<LLVMGenericValueRef> = Vec::new(); // no arguments
                        let res = LLVMRunFunction(engine, last_func, 0, args.as_mut_ptr());
                        println!("Evaluated to: {}", LLVMGenericValueToInt(res, true as i32));
                        LLVMRemoveModule(engine, module, &mut module, error);
                    }
                }
                Err(err) => println!("Error: {}", err),
            }
        }

        LLVMDisposeBuilder(builder);
        LLVMDisposeExecutionEngine(engine);
    }
}

/// Generated LLVM IR for the specified file.
/// # Panics
/// This method panics if `matches.value_of("input")` is `None`.
/// This method panics if the path does not exist. // TODO
fn compile_file(matches: ArgMatches) {
    unsafe {
        let path = matches.value_of("input").unwrap();
        let file_str = fs::read_to_string(path).unwrap();
        let source_file = Rc::new(SourceFile::new(path, file_str.as_str()));

        let context = LLVMGetGlobalContext();
        let module = LLVMModuleCreateWithNameInContext(c_str!(path), context);
        let builder = LLVMCreateBuilderInContext(context);

        let target_triple = LLVMGetDefaultTargetTriple();
        LLVMSetTarget(module, target_triple);

        let mut codegen_visitor = LlvmCodeGenVisitor::new(module, builder);
        let symbol_table;

        // codegen std.hoot
        let std_hoot = include_str!("../../owlc-passes/std.hoot");
        let std_source_file = Rc::new(SourceFile::new("std.hoot", std_hoot));
        let mut std_error_reporter = ErrorReporter::new(Rc::clone(&std_source_file));
        let mut std_lexer = Lexer::with_source_file(&std_source_file, &mut std_error_reporter);
        let mut parser_error_reporter = ErrorReporter::new(Rc::clone(&std_source_file));
        let mut parser = Parser::new(&mut std_lexer, &mut parser_error_reporter);
        let std_ast = parser.parse_compilation_unit();
        let mut resolver_visitor = ResolverVisitor::new(&mut std_error_reporter);
        resolver_visitor.visit_compilation_unit(&std_ast);
        symbol_table = resolver_visitor.symbols;

        // merge error reporter
        std_error_reporter.merge_from(&mut parser_error_reporter);
        if std_error_reporter.has_errors() {
            panic!("Errors in standard library. Aborting.");
        } else {
            codegen_visitor.visit_compilation_unit(&std_ast);
        }

        let mut error_reporter = ErrorReporter::new(Rc::clone(&source_file));
        let mut lexer_error_reporter = ErrorReporter::new(Rc::clone(&source_file));
        let mut lexer = Lexer::with_source_file(&source_file, &mut lexer_error_reporter);

        let ast = {
            let mut parser = Parser::new(&mut lexer, &mut error_reporter);
            let compilation_unit = parser.parse_compilation_unit();

            let mut resolver_visitor = ResolverVisitor::new(&mut error_reporter);
            resolver_visitor.symbols = symbol_table;
            resolver_visitor.visit_compilation_unit(&compilation_unit);

            compilation_unit
        };
        error_reporter.merge_from(&mut lexer_error_reporter);

        if error_reporter.has_errors() {
            print!("{}", error_reporter);
            return; // do not codegen if error
        }

        codegen_visitor.visit_compilation_unit(&ast);

        // optimization passes
        let pass_manager = LLVMCreatePassManager();
        LLVMAddPromoteMemoryToRegisterPass(pass_manager);
        LLVMRunPassManager(pass_manager, module);

        if matches.is_present("output") {
            // dump to file
            let out_file = matches.value_of("output").unwrap();
            LLVMWriteBitcodeToFile(module, c_str!(out_file));
        } else {
            // dump llvm ir to stdout
            LLVMDumpModule(module);
        }

        LLVMDisposeModule(module);
        LLVMDisposeBuilder(builder);
        LLVMDisposePassManager(pass_manager);
    }
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
            Arg::with_name("output")
                .short("o")
                .long("output")
                .help("Emits output to file or print to stdout if not present")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("dump-llvm")
                .long("dump-llvm")
                .help("Dumps generated LLVM IR to stdout in repl mode")
                .conflicts_with("input")
                .takes_value(false),
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
