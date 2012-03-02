(module code-gen racket
  (require
         racket/contract ; for provide/contract
         "racket-llvm/llvm.rkt" ; racket-llvm library
         "ast.rkt"
         )
  ;; racket-llvm/llvm.rkt depends on some C++ bindings.
  ;; Copy/symlink racket-llvm.Makefile to racket-llvm/Makefile and run make.
  ;; (Otherwise it throws an error complaining "llvm-racket.so" can't be found.)
  ;; If you then get a "wrong ELF class" error, change -m32 to -m64 (or viceversa) in the Makefile and recompile.
  
  ;; See also http://llvm.org/docs/CodeGenerator.html
  
  ;"Note that instead of adding virtual methods to [the AST], it could also make sense to use a visitor pattern
  ; or some other way to model this. Again, this tutorial won't dwell on good software engineering practices" - Kaleidoscope
  ; ^ This is what this module does: visit/map all nodes of the AST, emitting IR for each one.
  
  ; For external interface to this module:
  ;TODO: (provide/contract (code-gen (-> ((list?) (or/c path? string?) (boolean?) (boolean?)) (number?))))
(provide (all-defined-out))

; Used by webserver.rkt
(define (version)
  llvm-version-string)
  
(define (code-gen code-ast outfile verbose-mode exec-mode)

  (define llvm-dis "/usr/bin/llvm-dis") ; TODO: determine this dynamically.
  
  (define outfilename
    (cond
      ((path? outfile)
       (path->string outfile))
      (else outfile)))
  
  (define (debug message)
    (cond (verbose-mode (displayln message))))
  
  (debug (string-append "Generating code using LLVM version: " (version)))
  
  (define context (LLVMContextCreate)) ; LLVMGetGlobalContext
  
  ;"The LLVM construct that contains all of the functions and global variables in a chunk of code.
  ; In many ways, it is the top-level structure that the LLVM IR uses to contain code."
  (define module (LLVMModuleCreateWithNameInContext "neil-module" context)) ; TODO: What should this name be?
  
  ;"A helper object that makes it easy to generate LLVM instructions. [...]
  ;  keep track of the current place to insert instructions and has methods to create new instructions." - Kaleidoscope
  ;"An instruction builder represents a point within a basic block,
  ;  and is the exclusive means of building instructions using the C interface." - racket-llvm
  (define builder (LLVMCreateBuilderInContext context))
  
  ;"keeps track of which values are defined in the current scope and what their LLVM representation is.
  ;  (In other words, it is a symbol table for the code)." aka the environment
  (define env (make-hash)) ; string => Value*
  
  ; Trivial LLVM LISP also uses:
    ;ExistingModuleProvider provider;
	;FunctionPassManager    fpm;
  ;(define pass-manager (LLVMCreatePassManager))

  (define int-type (LLVMInt32TypeInContext context))
  (define zero (LLVMConstInt int-type 0 false))
  (define one (LLVMConstInt int-type 1 false))
  
  ;Codegen: ((Value*) ast->Codegen(cenv))->dump();
  (define (codegen-function func-name func-type)
    (define func
      (LLVMAddFunction module func-name func-type))
    ; TODO: dynamically, for each param declared in func-type:
    (define x (LLVMGetParam func 0))
    ;(define y (LLVMGetParam func 1))
    ;(define z (LLVMGetParam func 2))
    ; a block:
    (define entry (LLVMAppendBasicBlockInContext context func "entry"))
    ;create builder: (define builder (LLVMCreateBuilderInContext context))
    ; TODO: dynamically, for each param declared in func-type:
    (LLVMSetValueName x "x")
    ;(LLVMSetValueName y "y")
    ;(LLVMSetValueName z "z")
    ;place the builder
    (LLVMPositionBuilderAtEnd builder entry)
    ;body = return x*y+z
    (let* ((a (LLVMBuildMul builder x (LLVMConstInt int-type 2 false) "a")) ; const 2 instead of y
           (b (LLVMBuildAdd builder one a "b")) ; one instead of z
           )
      (LLVMBuildRet builder b))
    ; TODO: I think this is called only once at end, not for every block...
    ;(LLVMDisposeBuilder builder)
    func ; experimental: return func object to caller
  )
  
  ; Every program needs a function called "main"
  (define main
    (codegen-function "main" (LLVMFunctionType int-type (list int-type) 1)))
  
  (let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction))) ; This is where the 0 in the console output comes from.
    (when err
      (display err) (exit -1)))
  
  (LLVMLinkInJIT) ; or (LLVMLinkInInterpreter)
  (LLVMDisposeBuilder builder)
  
  (debug (string-append "Writing file: " outfilename))
  (LLVMWriteBitcodeToFile module outfilename)
  
  (when verbose-mode (LLVMDumpModule module)) ; to stderr
  ; Since LLVMDumpModule only writes to stderr, instead get LLVM IR by disassembling the bitcode file.
  (let-values (((process out in err) (subprocess #f #f #f llvm-dis outfilename))) ; *.bc file -> *.ll file
     (begin0
      (close-output-port in)
      (close-input-port err)
      (close-input-port out)
      (subprocess-wait process)
      ))
  
  ; run inline:
  (cond (exec-mode
    (let (
          (engine (LLVMCreateExecutionEngineForModule module))
          (arg1 (LLVMCreateGenericValueOfInt int-type 5 #t))
          )
      (let ((result (LLVMRunFunction engine main (list arg1))))
        (debug "Executing...")
        (LLVMGenericValueToInt result #t))))
    (else 0)) ; Return 0 for success.
  
  ) ; end code-gen
  
  
)
