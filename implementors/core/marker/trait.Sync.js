(function() {var implementors = {};
implementors["owlc_error"] = [{"text":"impl Sync for Error","synthetic":true,"types":[]},{"text":"impl !Sync for ErrorReporter","synthetic":true,"types":[]}];
implementors["owlc_lexer"] = [{"text":"impl&lt;'a&gt; !Sync for Lexer&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Sync for Token","synthetic":true,"types":[]},{"text":"impl Sync for TokenKind","synthetic":true,"types":[]}];
implementors["owlc_llvm"] = [{"text":"impl !Sync for LlvmCodeGenVisitor","synthetic":true,"types":[]}];
implementors["owlc_parser"] = [{"text":"impl Sync for Expr","synthetic":true,"types":[]},{"text":"impl Sync for ExprKind","synthetic":true,"types":[]},{"text":"impl Sync for CompilationUnit","synthetic":true,"types":[]},{"text":"impl Sync for Block","synthetic":true,"types":[]},{"text":"impl Sync for FnProto","synthetic":true,"types":[]},{"text":"impl Sync for Stmt","synthetic":true,"types":[]},{"text":"impl Sync for StmtKind","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Sync for Parser&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["owlc_passes"] = [{"text":"impl&lt;'a&gt; !Sync for MainFunctionVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Sync for SymbolTable","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Sync for ResolverVisitor&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Sync for Symbol","synthetic":true,"types":[]},{"text":"impl Sync for Scope","synthetic":true,"types":[]}];
implementors["owlc_source"] = [{"text":"impl !Sync for SourceFile","synthetic":true,"types":[]}];
implementors["owlc_span"] = [{"text":"impl Sync for BytePos","synthetic":true,"types":[]},{"text":"impl Sync for Span","synthetic":true,"types":[]},{"text":"impl !Sync for SourceFile","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()