(function() {var implementors = {};
implementors["owlc_error"] = [{"text":"impl Send for Error","synthetic":true,"types":[]},{"text":"impl !Send for ErrorReporter","synthetic":true,"types":[]}];
implementors["owlc_lexer"] = [{"text":"impl&lt;'a&gt; !Send for Lexer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for Token","synthetic":true,"types":[]},{"text":"impl Send for TokenKind","synthetic":true,"types":[]}];
implementors["owlc_llvm"] = [{"text":"impl !Send for LlvmCodeGenVisitor","synthetic":true,"types":[]}];
implementors["owlc_parser"] = [{"text":"impl Send for Expr","synthetic":true,"types":[]},{"text":"impl Send for ExprKind","synthetic":true,"types":[]},{"text":"impl Send for CompilationUnit","synthetic":true,"types":[]},{"text":"impl Send for Block","synthetic":true,"types":[]},{"text":"impl Send for FnProto","synthetic":true,"types":[]},{"text":"impl Send for Stmt","synthetic":true,"types":[]},{"text":"impl Send for StmtKind","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for Parser&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["owlc_passes"] = [{"text":"impl&lt;'a&gt; !Send for MainFunctionVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for SymbolTable","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for ResolverVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for Symbol","synthetic":true,"types":[]},{"text":"impl Send for Scope","synthetic":true,"types":[]}];
implementors["owlc_source"] = [{"text":"impl !Send for SourceFile","synthetic":true,"types":[]}];
implementors["owlc_span"] = [{"text":"impl Send for BytePos","synthetic":true,"types":[]},{"text":"impl Send for Span","synthetic":true,"types":[]},{"text":"impl !Send for SourceFile","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()