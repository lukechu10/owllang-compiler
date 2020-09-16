mod macros;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMIntPredicate, LLVMLinkage, LLVMTypeKind};
use owllang_lexer::TokenKind;
use owllang_parser::ast::expressions::*;
use owllang_parser::ast::statements::*;
use owllang_parser::visitor::AstVisitor;
use std::collections::HashMap;

pub struct LlvmCodeGenVisitor {
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,

    /// Separate builder for building `alloca` instructions in `entry` basic block.
    alloca_builder: LLVMBuilderRef,

    value_stack: Vec<LLVMValueRef>,
    named_values: HashMap<String, LLVMValueRef>,

    /// The current function. Should be set when visiting `FnStatementAST` and unset when exiting.
    current_function: Option<LLVMValueRef>,
}
impl LlvmCodeGenVisitor {
    pub fn new(module: LLVMModuleRef, builder: LLVMBuilderRef) -> Self {
        Self {
            module,
            builder,
            alloca_builder: unsafe { LLVMCreateBuilderInContext(LLVMGetGlobalContext()) },
            value_stack: Vec::new(),
            named_values: HashMap::new(),
            current_function: None,
        }
    }

    /// Creates an `alloca` instruction in the `entry` block of the current function. Returns the `LLVMValueRef` memory address of the allocated variable.
    pub unsafe fn build_entry_bb_alloca(&mut self, name: &String) -> LLVMValueRef {
        debug_assert!(self.current_function != None);

        let entry_bb = LLVMGetEntryBasicBlock(self.current_function.unwrap());
        let first_instr = LLVMGetFirstInstruction(entry_bb);

        if first_instr.is_null() {
            LLVMPositionBuilderAtEnd(self.alloca_builder, entry_bb);
        } else {
            LLVMPositionBuilderBefore(self.alloca_builder, first_instr);
        }

        LLVMBuildAlloca(self.alloca_builder, LLVMInt64Type(), c_str!(name))
    }
}

impl Drop for LlvmCodeGenVisitor {
    fn drop(&mut self) {
        unsafe {
            // dispose self.alloca_builder
            LLVMDisposeBuilder(self.alloca_builder);
        }
    }
}

/// Codegen functions for `Expr` variants
impl LlvmCodeGenVisitor {}

impl LlvmCodeGenVisitor {
    /// Custom codegen function for repl.
    pub fn handle_repl_input(&mut self, stmt: Stmt) {
        static mut ANON_FN_COUNTER: u64 = 0;

        match stmt.kind {
            StmtKind::Fn {
                ref proto,
                ref body,
            } => self.visit_fn_stmt(proto, body),
            StmtKind::Let {
                iden: _,
                initializer: _,
            } => unimplemented!(),
            StmtKind::ExprSemi { expr } => {
                // codegen anonymous function that returns value
                // create fake return stmt.
                unsafe {
                    let ret_stmt = Stmt::new(StmtKind::Return { value: expr });
                    let proto = FnProto {
                        iden: format!("0.repl.{}", ANON_FN_COUNTER), // start with '0' to prevent conflict with user defined functions
                        args: Vec::new(),
                    };
                    ANON_FN_COUNTER += 1;
                    let body = Block {
                        stmts: vec![ret_stmt],
                    };
                    self.visit_fn_stmt(&proto, &Some(body));
                }
            }
            _ => unreachable!(),
        }
    }
}

impl AstVisitor for LlvmCodeGenVisitor {
    fn visit_literal_expr(&mut self, val: &i64) {
        unsafe {
            let const_int = LLVMConstInt(LLVMInt64Type(), *val as u64, 0);
            self.value_stack.push(const_int);
        }
    }

    fn visit_identifier_expr(&mut self, ident: &String) {
        unsafe {
            match self.named_values.get(ident) {
                Some(addr) => {
                    // deallocate value
                    let val = LLVMBuildLoad(self.builder, *addr, c_str!("loadtmp"));
                    self.value_stack.push(val);
                }
                None => {
                    panic!(format!(
                        "Identifier {} does not exist in current scope",
                        ident
                    ));
                }
            }
        }
    }

    fn visit_func_call(&mut self, callee: &String, args: &Vec<Expr>) {
        unsafe {
            let arg_count = args.len();
            let func = LLVMGetNamedFunction(self.module, c_str!(callee));

            if func.is_null() {
                panic!("Error, function not defined");
            }
            if LLVMCountParams(func) != arg_count as u32 {
                panic!("Invalid argument count");
            }

            let mut argv: Vec<LLVMValueRef> = Vec::with_capacity(arg_count);

            for arg in args {
                self.visit_expr(arg);
                argv.push(self.value_stack.pop().unwrap()); // unwrap should not panic here because arg.accept pushes a value onto self.value_stack
            }

            let res = LLVMBuildCall(
                self.builder,
                func,
                argv.as_mut_ptr(),
                arg_count as u32,
                c_str!("calltmp"),
            );
            self.value_stack.push(res);
        }
    }

    fn visit_bin_expr(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, op_type: &TokenKind) {
        unsafe {
            let res: LLVMValueRef;

            // codegen right side of binary expression
            self.visit_expr(rhs);
            let rhs_llvm = self.value_stack.pop().unwrap();

            match &op_type {
                TokenKind::OpEquals => {
                    // do not codegen lhs for assignment expression
                    // lhs of assignment expression should be an identifier

                    let ident = {
                        match &lhs.kind {
                            ExprKind::Identifier(iden_str) => iden_str,
                            _ => panic!("LHS of assignment must be identifier"),
                        }
                    };

                    match self.named_values.get(ident) {
                        Some(addr_val) => {
                            debug_assert!(
                                LLVMGetTypeKind(LLVMTypeOf(*addr_val))
                                    == LLVMTypeKind::LLVMPointerTypeKind,
                                "Value should be pointer type."
                            );

                            res = LLVMBuildStore(self.builder, rhs_llvm, *addr_val);
                        }
                        None => {
                            panic!("Not in scope");
                        }
                    }
                }
                _ => {
                    // codegen lhs of binary expression
                    self.visit_expr(lhs);
                    let lhs_llvm = self.value_stack.pop().unwrap();

                    res = match &op_type {
                        TokenKind::OpPlus => {
                            LLVMBuildAdd(self.builder, lhs_llvm, rhs_llvm, c_str!("add_tmp"))
                        }
                        TokenKind::OpMinus => {
                            LLVMBuildSub(self.builder, lhs_llvm, rhs_llvm, c_str!("sub_tmp"))
                        }
                        TokenKind::OpAsterisk => {
                            LLVMBuildMul(self.builder, lhs_llvm, rhs_llvm, c_str!("mul_tmp"))
                        }
                        TokenKind::OpSlash => {
                            LLVMBuildSDiv(self.builder, lhs_llvm, rhs_llvm, c_str!("sdiv_tmp"))
                        }
                        TokenKind::OpPercent => {
                            LLVMBuildSRem(self.builder, lhs_llvm, rhs_llvm, c_str!("srem_tmp"))
                        }
                        _ => {
                            // codegen relational operators
                            let cmp_predicate = match &op_type {
                                TokenKind::OpEqualsEquals => {
                                    (LLVMIntPredicate::LLVMIntEQ, "eq_cmp_tmp")
                                }
                                TokenKind::OpGreaterThan => {
                                    (LLVMIntPredicate::LLVMIntSGT, "sgt_cmp_tmp")
                                }
                                TokenKind::OpEqualsGreaterThan => {
                                    (LLVMIntPredicate::LLVMIntSGE, "sge_cmp_tmp")
                                }
                                TokenKind::OpLessThan => {
                                    (LLVMIntPredicate::LLVMIntSLT, "slt_cmp_tmp")
                                }
                                TokenKind::OpEqualsLessThan => {
                                    (LLVMIntPredicate::LLVMIntSLE, "sle_cmp_tmp")
                                }
                                _ => unreachable!(),
                            };

                            let cmp_tmp = LLVMBuildICmp(
                                self.builder,
                                cmp_predicate.0,
                                lhs_llvm,
                                rhs_llvm,
                                c_str!(cmp_predicate.1),
                            );
                            LLVMBuildIntCast(
                                self.builder,
                                cmp_tmp,
                                LLVMInt64Type(),
                                c_str!("cast_tmp"),
                            )
                        }
                    };
                }
            }

            self.value_stack.push(res);
        }
    }

    fn visit_fn_proto(&mut self, ident: &String, args: &Vec<String>) {
        unsafe {
            let arg_count = args.len();
            let mut argv: Vec<LLVMTypeRef> = Vec::with_capacity(arg_count);

            let mut func = LLVMGetNamedFunction(self.module, c_str!(ident));

            if !func.is_null() && LLVMGetEntryBasicBlock(func).is_null() {
                panic!("Error, function '{}' already exists.", ident);
            }

            for _ in 0..args.len() {
                argv.push(LLVMInt64Type());
            }

            let func_type = LLVMFunctionType(
                LLVMInt64Type(),
                argv.as_mut_ptr(),
                arg_count as u32,
                false as i32,
            );

            func = LLVMAddFunction(self.module, c_str!(ident), func_type);
            LLVMSetLinkage(func, LLVMLinkage::LLVMExternalLinkage);

            self.value_stack.push(func);
        }
    }

    fn visit_fn_stmt(&mut self, proto: &FnProto, body: &Option<Block>) {
        unsafe {
            self.named_values.clear(); // clear symbols from previous function

            self.visit_fn_proto(&proto.iden, &proto.args);
            let func = self.value_stack.pop().unwrap();

            match body {
                Some(body) => {
                    self.current_function = Some(func);

                    LLVMPositionBuilderAtEnd(
                        self.builder,
                        LLVMAppendBasicBlock(func, c_str!("entry")),
                    );

                    for (i, arg) in proto.args.iter().enumerate() {
                        let addr = self.build_entry_bb_alloca(arg);
                        // store value
                        LLVMBuildStore(self.builder, LLVMGetParam(func, i as u32), addr);
                        self.named_values.insert(arg.clone(), addr);
                    }

                    // codegen function body
                    self.visit_block(body);

                    // unset self.current_function
                    self.current_function = None;
                }
                None => {}
            }
        }
    }

    fn visit_let_stmt(&mut self, ident: &String, initializer: &Expr) {
        assert!(self.current_function.is_some());

        if self.named_values.contains_key(ident) {
            panic!("Variable {} already exists.", ident);
        }

        unsafe {
            let alloca_ptr = self.build_entry_bb_alloca(ident);
            // codegen initializer expr
            self.visit_expr(initializer);
            // build load
            LLVMBuildStore(self.builder, self.value_stack.pop().unwrap(), alloca_ptr);

            // add variable to named_values
            self.named_values.insert(ident.clone(), alloca_ptr);
        }
    }

    fn visit_return_stmt(&mut self, value: &Expr) {
        unsafe {
            self.visit_expr(value); // codegen return value
            let llvm_value = self.value_stack.pop().unwrap();
            LLVMBuildRet(self.builder, llvm_value);
        }
    }

    fn visit_expr_semi_stmt(&mut self, expr: &Expr) {
        self.visit_expr(expr);
        self.value_stack.pop(); // make value is not accessible
    }
}
