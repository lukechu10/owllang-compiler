(function() {var implementors = {};
implementors["owlc_error"] = [{"text":"impl Unpin for Error","synthetic":true,"types":[]},{"text":"impl Unpin for ErrorReporter","synthetic":true,"types":[]}];
implementors["owlc_lexer"] = [{"text":"impl&lt;'a&gt; Unpin for Lexer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for Token","synthetic":true,"types":[]},{"text":"impl Unpin for TokenKind","synthetic":true,"types":[]}];
implementors["owlc_llvm"] = [{"text":"impl Unpin for LlvmCodeGenVisitor","synthetic":true,"types":[]}];
implementors["owlc_parser"] = [{"text":"impl Unpin for Expr","synthetic":true,"types":[]},{"text":"impl Unpin for ExprKind","synthetic":true,"types":[]},{"text":"impl Unpin for CompilationUnit","synthetic":true,"types":[]},{"text":"impl Unpin for Block","synthetic":true,"types":[]},{"text":"impl Unpin for FnProto","synthetic":true,"types":[]},{"text":"impl Unpin for Stmt","synthetic":true,"types":[]},{"text":"impl Unpin for StmtKind","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Unpin for Parser&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["owlc_passes"] = [{"text":"impl&lt;'a&gt; Unpin for MainFunctionVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for SymbolTable","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Unpin for ResolverVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for Symbol","synthetic":true,"types":[]},{"text":"impl Unpin for Scope","synthetic":true,"types":[]}];
implementors["owlc_source"] = [{"text":"impl Unpin for SourceFile","synthetic":true,"types":[]}];
implementors["owlc_span"] = [{"text":"impl Unpin for BytePos","synthetic":true,"types":[]},{"text":"impl Unpin for Span","synthetic":true,"types":[]},{"text":"impl Unpin for SourceFile","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()