(function() {var implementors = {};
implementors["owlc_error"] = [{"text":"impl UnwindSafe for Error","synthetic":true,"types":[]},{"text":"impl !UnwindSafe for ErrorReporter","synthetic":true,"types":[]}];
implementors["owlc_lexer"] = [{"text":"impl&lt;'a&gt; !UnwindSafe for Lexer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Token","synthetic":true,"types":[]},{"text":"impl UnwindSafe for TokenKind","synthetic":true,"types":[]}];
implementors["owlc_llvm"] = [{"text":"impl UnwindSafe for LlvmCodeGenVisitor","synthetic":true,"types":[]}];
implementors["owlc_parser"] = [{"text":"impl UnwindSafe for Expr","synthetic":true,"types":[]},{"text":"impl UnwindSafe for ExprKind","synthetic":true,"types":[]},{"text":"impl UnwindSafe for CompilationUnit","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Block","synthetic":true,"types":[]},{"text":"impl UnwindSafe for FnProto","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Stmt","synthetic":true,"types":[]},{"text":"impl UnwindSafe for StmtKind","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !UnwindSafe for Parser&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["owlc_passes"] = [{"text":"impl&lt;'a&gt; !UnwindSafe for MainFunctionVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SymbolTable","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !UnwindSafe for ResolverVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Symbol","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Scope","synthetic":true,"types":[]}];
implementors["owlc_source"] = [{"text":"impl !UnwindSafe for SourceFile","synthetic":true,"types":[]}];
implementors["owlc_span"] = [{"text":"impl UnwindSafe for BytePos","synthetic":true,"types":[]},{"text":"impl UnwindSafe for Span","synthetic":true,"types":[]},{"text":"impl UnwindSafe for SourceFile","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()