use clap::{App, Arg, ArgMatches};
use llvm_sys::{
    analysis::*, bit_writer::*, core::*, execution_engine::*, target::*, target_machine::*,
    transforms::util::*,
};
use owlc_error::ErrorReporter;
use owlc_span::SourceFile;
use owlc_passes::resolver::{ResolverVisitor, SymbolTable};
use owllang_lexer::Lexer;
use owllang_llvm_codegen::{c_str, LlvmCodeGenVisitor};
use owllang_parser::{ast::statements::StmtKind, parser::Parser, Visitor};
use std::{fs, io, io::prelude::*};

fn repl_loop(matches: &ArgMatches) {
    unsafe {
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        let context = LLVMGetGlobalContext();
        let mut module = LLVMModuleCreateWithNameInContext(c_str!("repl"), context);
        let builder = LLVMCreateBuilderInContext(context);
        let mut codegen_visitor = LlvmCodeGenVisitor::new(module, builder);
        codegen_visitor.add_builtin_fns();

        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();

        let mut engine: LLVMExecutionEngineRef = std::ptr::null_mut();
        let error: *mut *mut i8 = std::ptr::null_mut();
        LLVMCreateExecutionEngineForModule(&mut engine, module, error);

        let target_machine = LLVMGetExecutionEngineTargetMachine(engine);
        let target_data_layout = LLVMCreateTargetDataLayout(target_machine);
        LLVMSetModuleDataLayout(module, target_data_layout);

        let mut symbol_table = SymbolTable::new();

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

                    let mut error_reporter = ErrorReporter::new();
                    let mut lexer_error_reporter = ErrorReporter::new();
                    let source_file = SourceFile::new("<repl>",input.as_str());
                    let mut lexer = Lexer::with_source_file(&source_file, &mut lexer_error_reporter);

                    let ast = {
                        let mut parser = Parser::new(&mut lexer, &mut error_reporter);
                        let stmt = parser.parse_repl_input();

                        let mut resolver_visitor = ResolverVisitor::new(&mut error_reporter);
                        // restore previous symbols
                        resolver_visitor.symbols = symbol_table;

                        resolver_visitor.visit_stmt(&stmt).unwrap();

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

                    codegen_visitor.handle_repl_input(ast).unwrap();

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
    let path = matches.value_of("input").unwrap();
    let file_str = fs::read_to_string(path).unwrap();
    let source_file = SourceFile::new(path, file_str.as_str());

    let mut error_reporter = ErrorReporter::new();
    let mut lexer_error_reporter = ErrorReporter::new();
    let mut lexer = Lexer::with_source_file(&source_file, &mut lexer_error_reporter);

    let ast = {
        let mut parser = Parser::new(&mut lexer, &mut error_reporter);
        let compilation_unit = parser.parse_compilation_unit();

        let mut resolver_visitor = ResolverVisitor::new(&mut error_reporter);

        resolver_visitor
            .visit_compilation_unit(&compilation_unit)
            .unwrap();

        compilation_unit
    };
    error_reporter.merge_from(&mut lexer_error_reporter);

    if error_reporter.has_errors() {
        print!("{}", error_reporter);
        return; // do not codegen if error
    }

    unsafe {
        let context = LLVMGetGlobalContext();
        let module = LLVMModuleCreateWithNameInContext(c_str!(path), context);
        let builder = LLVMCreateBuilderInContext(context);

        let target_triple = LLVMGetDefaultTargetTriple();
        LLVMSetTarget(module, target_triple);

        let mut codegen_visitor = LlvmCodeGenVisitor::new(module, builder);
        codegen_visitor.add_builtin_fns();
        match codegen_visitor.visit_compilation_unit(&ast) {
            Ok(_) => {}
            Err(err) => {
                println!("{}", err.message);
                return;
            }
        }

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
