//! Driver code for owlc-cli
use ansi_term::{Color, Style};
use clap::{App, Arg, ArgMatches};
use llvm_sys::support::*;
use llvm_sys::{
    analysis::*, bit_writer::*, core::*, execution_engine::*, target::*, target_machine::*,
    transforms, transforms::util::*,
};
use owlc_error::ErrorReporter;
use owlc_lexer::{Lexer, TokenKind};
use owlc_llvm::{c_str, LlvmCodeGenVisitor};
use owlc_parser::{ast::statements::StmtKind, parser::Parser, visitor::AstVisitor};
use owlc_passes::{fn_main::MainFunctionVisitor, resolver::ResolverVisitor};
use owlc_span::SourceFile;
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

/// Returns `true` if the repl input is finished.
fn repl_input_is_finished(buf: &str) -> bool {
    let mut paren_count = 0;
    let mut brace_count = 0;

    for char in buf.chars() {
        match char {
            '(' => paren_count += 1,
            ')' => paren_count -= 1,
            '{' => brace_count += 1,
            '}' => brace_count -= 1,
            _ => {}
        }

        if paren_count < 0 || brace_count < 0 {
            return true; // paren_count and brace_count should be >= 0. Otherwise is error.
        }
    }

    paren_count == 0 && brace_count == 0
}

/// Reads a line from the repl input. Automatically detects if input is finished (balanced paren and brace operators) and prompts user for second line if not.
fn get_repl_input() -> String {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut buf = String::new();
    // Current indentation level. Incremented when last character is '(' or '{'. Decremented when string contains ')' or '}'.
    let mut indent: i32 = 0;
    match stdin.read_line(&mut buf) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error while reading input: {}", err);
        }
    } // initial input

    while !repl_input_is_finished(&buf) {
        let mut tmp = String::new();

        print!("{} ", ".".repeat((5 + (4 * indent)) as usize));
        stdout.flush().unwrap();
        match stdin.read_line(&mut tmp) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error while reading input: {}", err);
            }
        }

        if tmp.find(|c| c == '(' || c == '{').is_some() {
            indent += 1;
        }
        if tmp.find(|c| c == ')' || c == '}').is_some() {
            indent -= std::cmp::max(1, 0); // at least 0
        }

        buf += tmp.as_str();
    }

    buf
}

fn repl_loop(matches: &ArgMatches) {
    unsafe {
        LLVMAddSymbol(c_str!("println"), println as *mut std::ffi::c_void);
        LLVMAddSymbol(c_str!("read_num"), read_num as *mut std::ffi::c_void);

        let mut stdout = io::stdout();

        let context = LLVMGetGlobalContext();
        let mut module = LLVMModuleCreateWithNameInContext(c_str!("<repl>"), context);
        let builder = LLVMCreateBuilderInContext(context);
        let mut codegen_visitor = LlvmCodeGenVisitor::new(module, builder);

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
        std_error_reporter.merge_from(&parser_error_reporter);
        if std_error_reporter.has_errors() {
            panic!("Errors in standard library. Aborting.");
        } else {
            codegen_visitor.visit_compilation_unit(&std_ast);
        }

        loop {
            print!("{}", Style::default().dimmed().paint("> "));
            stdout.flush().unwrap();

            let input = get_repl_input();
            if input == ".quit\n" {
                // exit repl
                println!("Exiting repl.");
                break;
            }

            let source_file = Rc::new(SourceFile::new("<repl>", input.as_str()));
            let mut error_reporter = ErrorReporter::new(Rc::clone(&source_file));
            let mut lexer_error_reporter = ErrorReporter::new(Rc::clone(&source_file));
            let mut lexer = Lexer::with_source_file(&source_file, &mut lexer_error_reporter);

            if matches.is_present("show-lex") {
                let tokens: Vec<TokenKind> = lexer.map(|token| token.kind).collect();
                println!("{:?}", tokens);
                continue; // end
            }

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

            error_reporter.merge_from(&lexer_error_reporter);

            if error_reporter.has_errors() {
                print!("{}", error_reporter);
                continue; // do not evaluate result
            }

            if matches.is_present("show-ast") {
                let serialize_res = serde_yaml::to_string(&ast).unwrap();
                println!("{}", serialize_res);
            }

            // True if repl should evaluate input.
            let evaluate_res = match ast.kind {
                StmtKind::ExprSemi { expr: _ } => true,
                _ => false,
            };

            codegen_visitor.handle_repl_input(ast);

            let last_func = LLVMGetLastFunction(module);
            LLVMVerifyFunction(last_func, LLVMVerifierFailureAction::LLVMPrintMessageAction);
            // optimization passes
            let pm = LLVMCreatePassManager();
            LLVMAddPromoteMemoryToRegisterPass(pm);
            transforms::scalar::LLVMAddTailCallEliminationPass(pm);
            transforms::instcombine::LLVMAddInstructionCombiningPass(pm);
            transforms::scalar::LLVMAddReassociatePass(pm);
            transforms::scalar::LLVMAddGVNPass(pm);
            transforms::scalar::LLVMAddCFGSimplificationPass(pm);
            LLVMRunPassManager(pm, module);
            LLVMDisposePassManager(pm);

            if matches.is_present("dump-llvm") {
                LLVMDumpValue(last_func);
            }

            if evaluate_res {
                LLVMAddModule(engine, module);
                let mut args: Vec<LLVMGenericValueRef> = Vec::new(); // no arguments
                let res = LLVMRunFunction(engine, last_func, 0, args.as_mut_ptr());
                println!(
                    "{}",
                    Color::Yellow.paint((LLVMGenericValueToInt(res, 1) as i64).to_string())
                );
                LLVMRemoveModule(engine, module, &mut module, error);
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
        std_error_reporter.merge_from(&parser_error_reporter);
        if std_error_reporter.has_errors() {
            panic!("Errors in standard library. Aborting.");
        } else {
            codegen_visitor.visit_compilation_unit(&std_ast);
        }

        let mut error_reporter = ErrorReporter::new(Rc::clone(&source_file));
        let mut lexer_error_reporter = ErrorReporter::new(Rc::clone(&source_file));
        let mut lexer = Lexer::with_source_file(&source_file, &mut lexer_error_reporter);

        if matches.is_present("show-lex") {
            let tokens: Vec<TokenKind> = lexer.map(|token| token.kind).collect();
            println!("{:?}", tokens);
            return; // end
        }

        let ast = {
            let mut parser = Parser::new(&mut lexer, &mut error_reporter);
            let compilation_unit = parser.parse_compilation_unit();

            let mut resolver_visitor = ResolverVisitor::new(&mut error_reporter);
            resolver_visitor.symbols = symbol_table;
            resolver_visitor.visit_compilation_unit(&compilation_unit);

            let mut fn_main_visitor = MainFunctionVisitor::new(&mut error_reporter);
            fn_main_visitor.visit_compilation_unit(&compilation_unit);

            compilation_unit
        };

        if matches.is_present("show-ast") {
            println!("{:#?}", ast);
        }

        error_reporter.merge_from(&lexer_error_reporter);

        if error_reporter.has_errors() {
            print!("{}", error_reporter);
            return; // do not codegen if error
        }

        codegen_visitor.visit_compilation_unit(&ast);

        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMPrintMessageAction,
            std::ptr::null_mut::<*mut i8>(),
        );

        // optimization passes
        let pass_manager = LLVMCreatePassManager();
        LLVMAddPromoteMemoryToRegisterPass(pass_manager);
        transforms::instcombine::LLVMAddInstructionCombiningPass(pass_manager);
        transforms::scalar::LLVMAddReassociatePass(pass_manager);
        transforms::scalar::LLVMAddGVNPass(pass_manager);
        transforms::scalar::LLVMAddCFGSimplificationPass(pass_manager);
        transforms::scalar::LLVMAddTailCallEliminationPass(pass_manager);
        LLVMRunPassManager(pass_manager, module);

        if matches.is_present("output") {
            // dump to file
            let out_file = matches.value_of("output").unwrap();
            LLVMWriteBitcodeToFile(module, c_str!(out_file));
        } else if matches.is_present("dump-llvm") {
            // dump llvm ir to stdout
            LLVMDumpModule(module);
        } else {
            // run in jit
            LLVMLinkInMCJIT();
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            let mut engine: LLVMExecutionEngineRef = std::ptr::null_mut();
            let error: *mut *mut i8 = std::ptr::null_mut();
            LLVMCreateExecutionEngineForModule(&mut engine, module, error);

            let target_machine = LLVMGetExecutionEngineTargetMachine(engine);
            let target_data_layout = LLVMCreateTargetDataLayout(target_machine);
            LLVMSetModuleDataLayout(module, target_data_layout);

            LLVMAddSymbol(c_str!("println"), println as *mut std::ffi::c_void);
            LLVMAddSymbol(c_str!("read_num"), read_num as *mut std::ffi::c_void);

            LLVMAddModule(engine, module);

            // Get main function
            let main_func = LLVMGetNamedFunction(module, c_str!("main"));

            let mut args: Vec<LLVMGenericValueRef> = Vec::new(); // no arguments
            LLVMRunFunction(engine, main_func, 0, args.as_mut_ptr());
        }

        LLVMDisposeModule(module);
        LLVMDisposeBuilder(builder);
        LLVMDisposePassManager(pass_manager);
    }
}

pub fn driver() {
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
                .help("Dumps generated LLVM IR to stdout in repl mode. If compiling a file, will dump LLVM IR instead of executing it in JIT.")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Shows the generated abstract syntax tree")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("show-lex")
                .long("show-lex")
                .help("Shows the lexer output. Note: this option will prevent the code from being parsed.")
                .conflicts_with_all(&["show-ast", "dump-llvm", "output"])
                .takes_value(false)
        )
        .get_matches();

    match matches.value_of("input") {
        Some(_) => compile_file(matches),
        None => repl_loop(&matches),
    }
}
