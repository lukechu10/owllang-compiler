mod macros;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::transforms::util::*;
use llvm_sys::{LLVMIntPredicate, LLVMLinkage, LLVMTypeKind};
use owllang_lexer::TokenVal;
use owllang_parser::ast::expressions::*;
use owllang_parser::ast::statements::*;
use owllang_parser::{SyntaxError, Visitor};
use std::collections::HashMap;

pub struct LlvmCodeGenVisitor {
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,

    /// Seperate builder for building `alloca` instructions in `entry` basic block.
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

    /// Creates an `alloca` instruction in the `entry` block of the current function. Returns the memory address of the allocated variable.
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
impl LlvmCodeGenVisitor {
    fn visit_literal_expr(&mut self, node: &Expr) -> Result<(), SyntaxError> {
        unsafe {
            match &node.kind {
                ExprKind::Literal(num) => {
                    let const_int = LLVMConstInt(LLVMInt64Type(), *num as u64, 0);
                    self.value_stack.push(const_int);
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn visit_identifier_expr(&mut self, node: &Expr) -> Result<(), SyntaxError> {
        unsafe {
            match &node.kind {
                ExprKind::Identifier(iden) => {
                    let value_option = self.named_values.get(iden);

                    if let Some(value) = value_option {
                        // deallocate value
                        let val = LLVMBuildLoad(self.builder, *value, c_str!("loadtmp"));
                        self.value_stack.push(val);
                        Ok(())
                    } else {
                        Err(SyntaxError {
                            // TODO
                            row: 0,
                            col: 0,
                            file_name: "tmp".to_string(),
                            message: format!(
                                "Identifier {} does not exist in current context",
                                iden
                            ),
                        })
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    fn visit_call_expr(&mut self, node: &Expr) -> Result<(), SyntaxError> {
        unsafe {
            match &node.kind {
                ExprKind::FuncCall { callee, args } => {
                    let arg_count = args.len();
                    let func = LLVMGetNamedFunction(self.module, c_str!(callee));

                    if func.is_null() {
                        panic!("Error");
                    }
                    if LLVMCountParams(func) != arg_count as u32 {
                        panic!("Invalid argument count");
                    }

                    let mut argv: Vec<LLVMValueRef> = Vec::with_capacity(arg_count);

                    for arg in args {
                        self.visit_expr(arg)?;
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

                    Ok(())
                }
                _ => unreachable!(),
            }
        }
    }

    fn visit_binary_expr(&mut self, node: &Expr) -> Result<(), SyntaxError> {
        unsafe {
            match &node.kind {
                ExprKind::BinaryExpr {
                    lhs: lhs_ast,
                    rhs: rhs_ast,
                    op_type,
                } => {
                    let res: LLVMValueRef;

                    // codegen right side of binary expression
                    self.visit_expr(rhs_ast)?;
                    let rhs = self.value_stack.pop().unwrap();

                    match &op_type {
                        TokenVal::OpEquals => {
                            // do not codegen lhs for assignment expression
                            // lhs of assignment expression should be an identifier

                            let iden = {
                                match &lhs_ast.kind {
                                    ExprKind::Identifier(iden_str) => iden_str,
                                    _ => panic!("LHS of assignment must be identifier"),
                                }
                            };

                            match self.named_values.get(iden) {
                                Some(addr_val) => {
                                    debug_assert!(
                                        LLVMGetTypeKind(LLVMTypeOf(*addr_val))
                                            == LLVMTypeKind::LLVMPointerTypeKind,
                                        "Value should be pointer type."
                                    );

                                    res = LLVMBuildStore(self.builder, rhs, *addr_val);
                                }
                                None => {
                                    panic!("Not in scope");
                                }
                            }
                        }
                        _ => {
                            // codegen lhs of binary expression
                            self.visit_expr(lhs_ast)?;
                            let lhs = self.value_stack.pop().unwrap();

                            res = match &op_type {
                                TokenVal::OpPlus => {
                                    LLVMBuildAdd(self.builder, lhs, rhs, c_str!("add_tmp"))
                                }
                                TokenVal::OpMinus => {
                                    LLVMBuildSub(self.builder, lhs, rhs, c_str!("sub_tmp"))
                                }
                                TokenVal::OpAsterisk => {
                                    LLVMBuildMul(self.builder, lhs, rhs, c_str!("mul_tmp"))
                                }
                                TokenVal::OpSlash => {
                                    LLVMBuildSDiv(self.builder, lhs, rhs, c_str!("sdiv_tmp"))
                                }
                                TokenVal::OpPercent => {
                                    LLVMBuildSRem(self.builder, lhs, rhs, c_str!("srem_tmp"))
                                }
                                _ => {
                                    // codegen relational operators
                                    let cmp_predicate = match &op_type {
                                        TokenVal::OpEqualsEquals => {
                                            (LLVMIntPredicate::LLVMIntEQ, "eq_cmp_tmp")
                                        }
                                        TokenVal::OpGreaterThan => {
                                            (LLVMIntPredicate::LLVMIntSGT, "sgt_cmp_tmp")
                                        }
                                        TokenVal::OpEqualsGreaterThan => {
                                            (LLVMIntPredicate::LLVMIntSGE, "sge_cmp_tmp")
                                        }
                                        TokenVal::OpLessThan => {
                                            (LLVMIntPredicate::LLVMIntSLT, "slt_cmp_tmp")
                                        }
                                        TokenVal::OpEqualsLessThan => {
                                            (LLVMIntPredicate::LLVMIntSLE, "sle_cmp_tmp")
                                        }
                                        _ => unreachable!(),
                                    };

                                    let cmp_tmp = LLVMBuildICmp(
                                        self.builder,
                                        cmp_predicate.0,
                                        lhs,
                                        rhs,
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
                    Ok(())
                }
                _ => unreachable!(),
            }
        }
    }
}

impl LlvmCodeGenVisitor {
    fn codegen_fn_proto(&mut self, node: &FnProto) -> Result<(), SyntaxError> {
        unsafe {
            let arg_count = node.args.len();
            let mut argv: Vec<LLVMTypeRef> = Vec::with_capacity(arg_count);

            let mut func = LLVMGetNamedFunction(self.module, c_str!(node.iden));

            if !func.is_null() {
                panic!("Error, function already exists.");
            }

            for _ in &node.args {
                argv.push(LLVMInt64Type());
            }

            let func_type = LLVMFunctionType(
                LLVMInt64Type(),
                argv.as_mut_ptr(),
                arg_count as u32,
                false as i32,
            );

            func = LLVMAddFunction(self.module, c_str!(node.iden), func_type);
            LLVMSetLinkage(func, LLVMLinkage::LLVMExternalLinkage);

            self.value_stack.push(func);

            Ok(())
        }
    }

    /// # Arguments
    /// * `statements` - The `statements` field in `SemiKind::Block`.
    fn codegen_block_stmt(&mut self, statements: &Vec<Stmt>) -> Result<(), SyntaxError> {
        for stmt in statements {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    /// # Arguments
    /// * `proto` - The function prototype.
    /// * `body` - The function body.
    fn codegen_fn_stmt(&mut self, proto: &FnProto, body: &Box<Stmt>) -> Result<(), SyntaxError> {
        unsafe {
            self.named_values.clear(); // clear symbols from previous function

            self.codegen_fn_proto(proto)?;
            let func = self.value_stack.pop().unwrap();

            self.current_function = Some(func);

            LLVMPositionBuilderAtEnd(self.builder, LLVMAppendBasicBlock(func, c_str!("entry")));

            for (i, arg) in proto.args.iter().enumerate() {
                let addr = self.build_entry_bb_alloca(arg);
                // store value
                LLVMBuildStore(self.builder, LLVMGetParam(func, i as u32), addr);
                self.named_values.insert(arg.clone(), addr);
            }

            // codegen function body
            match &body.kind {
                StmtKind::Block { statements } => self.codegen_block_stmt(statements)?,
                _ => unreachable!(),
            }

            // unset self.current_function
            self.current_function = None;
            Ok(())
        }
    }

    fn codegen_let_stmt(&mut self, _iden: &String, _initializer: &Expr) -> Result<(), SyntaxError> {
        unimplemented!()
    }

    /// # Arguments
    /// * `value` - The return value expression of the `SemiKind::Return` node.
    fn codegen_return_stmt(&mut self, value: &Expr) -> Result<(), SyntaxError> {
        unsafe {
            self.visit_expr(value)?; // codegen return value
            let llvm_value = self.value_stack.pop().unwrap();
            LLVMBuildRet(self.builder, llvm_value);

            Ok(())
        }
    }

    /// # Arguments
    /// * `expr` - The expression of the `SemiKind::ExprSemi` node.
    fn codegen_expr_semi_stmt(&mut self, expr: &Expr) -> Result<(), SyntaxError> {
        self.visit_expr(expr)?;
        self.value_stack.pop(); // make value is not accessible
        Ok(())
    }

    /// Custom codegen function for repl.
    pub fn handle_repl_input(&mut self, stmt: Stmt) -> Result<(), SyntaxError> {
        match stmt.kind {
            StmtKind::Fn {
                ref proto,
                ref body,
            } => self.codegen_fn_stmt(proto, body),
            StmtKind::Let {
                iden: _,
                initializer: _,
            } => unimplemented!(),
            StmtKind::ExprSemi { expr } => {
                // codegen anonymous function that returns value
                // create fake return stmt.
                let ret_stmt = Stmt::new(StmtKind::Return { value: expr });
                let proto = FnProto {
                    iden: "".to_string(),
                    args: Vec::new(),
                };
                let body = Stmt::new(StmtKind::Block {
                    statements: vec![ret_stmt],
                });
                self.codegen_fn_stmt(&proto, &Box::new(body))
            }
            _ => unreachable!(),
        }
    }
}

impl Visitor for LlvmCodeGenVisitor {
    fn visit_expr(&mut self, node: &Expr) -> Result<(), SyntaxError> {
        match &node.kind {
            ExprKind::Literal(_) => self.visit_literal_expr(node),
            ExprKind::Identifier(_) => self.visit_identifier_expr(node),
            ExprKind::FuncCall { callee: _, args: _ } => self.visit_call_expr(node),
            ExprKind::BinaryExpr {
                lhs: _,
                rhs: _,
                op_type: _,
            } => self.visit_binary_expr(node),
        }
    }

    fn visit_stmt(&mut self, node: &Stmt) -> Result<(), SyntaxError> {
        match &node.kind {
            StmtKind::Block { statements } => self.codegen_block_stmt(statements),
            StmtKind::Fn { proto, body } => self.codegen_fn_stmt(proto, body),
            StmtKind::Let { iden, initializer } => self.codegen_let_stmt(iden, initializer),
            StmtKind::Return { value } => self.codegen_return_stmt(value),
            StmtKind::ExprSemi { expr } => self.codegen_expr_semi_stmt(expr),
            _ => unimplemented!(),
        }
    }

    fn visit_compilation_unit(&mut self, node: &CompilationUnit) -> Result<(), SyntaxError> {
        // visit functions in compilation unit
        for function in &node.functions {
            self.visit_stmt(function)?;
        }
        Ok(())
    }
}

pub fn codegen_compilation_unit(ast: &CompilationUnit) -> Result<LLVMModuleRef, SyntaxError> {
    unsafe {
        let context = LLVMGetGlobalContext();
        let module_name = ast.entry_file_name.as_str();
        let module = LLVMModuleCreateWithNameInContext(c_str!(module_name), context);
        let builder = LLVMCreateBuilderInContext(context);

        let mut code_gen_visitor = LlvmCodeGenVisitor::new(module, builder);
        code_gen_visitor.visit_compilation_unit(ast)?;

        // basic optimizations
        let pass_manager = LLVMCreatePassManager();
        LLVMAddPromoteMemoryToRegisterPass(pass_manager);
        LLVMRunPassManager(pass_manager, module);

        LLVMDumpModule(module);

        // dispose resources to prevent memory leak
        LLVMDisposeModule(module);
        LLVMDisposeBuilder(builder);
        LLVMDisposePassManager(pass_manager);

        Ok(module)
    }
}
