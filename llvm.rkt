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

  ; *.bc file -> *.ll file
  (define (llvm-dis filename)
    (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/llvm-dis" filename)))
     (begin0
      (close-output-port in)
      (close-input-port err)
      (close-input-port out)
      (subprocess-wait process)
      )))
  
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
  
  ; Type abbreviation
  (define int-type (LLVMInt32TypeInContext context))
  
  ; All numbers are int32's for now
  (define (gen-number n)
    (LLVMConstInt (LLVMInt32TypeInContext context) n false))

  (define (gen-string s)
     (LLVMConstString s false)) ;TODO: InContext?

  ; Variable reference
  (define (gen-var name)
    (let ((var (hash-ref env name (lambda () (error "Undefined identifier: ~a" name)))))
          var)) ; TODO: add cutom type-tag to differentiate between Value and Function? May not have to: instead: TheModule->getFunction(Callee)
  
  ; Variable definition
  (define (gen-def name val)
    (hash-set! env name val))
  
  (define (gen-primative op L R)
    ;TODO: eval/apply L and R first?
    (case op
      ('+ (LLVMBuildAdd builder L R "addtmp"))
      ('- (LLVMBuildSub builder L R "subtmp"))
      ('* (LLVMBuildMul builder L R "multmp"))
      ('/ (LLVMBuildSDiv builder L R "divtmp"))
      ('% (LLVMBuildSRem builder L R "modtmp"))
      ('& (LLVMBuildAnd builder L R "andtmp"))
      ('or (LLVMBuildOr builder L R "ortmp"))
      ('^ (LLVMBuildXor builder L R "xortmp"))
      ('~ (LLVMBuildNot builder L R "nottmp"))
      ('<< (LLVMBuildShl builder L R "shltmp"))
      ('>> (LLVMBuildAShr builder L R "shrtmp")) ;TODO: or LShr?
      ('= (LLVMBuildICmp builder 'LLVMIntEQ L R "cmptmp"))
      ('<> (LLVMBuildICmp builder 'LLVMIntNE L R "cmptmp"))
      ('< (LLVMBuildICmp builder 'LLVMIntSLT L R "cmptmp"))
      ('<= (LLVMBuildICmp builder 'LLVMIntSLE L R "cmptmp"))
      ('> (LLVMBuildICmp builder 'LLVMIntSGT L R "cmptmp"))
      ('>= (LLVMBuildICmp builder 'LLVMIntSGE L R "cmptmp"))
      (else error "gen-primative: invalid primative: ~a" op)))

  (define (gen-call fname args)
    ;Look up the name in the global module table.
    (let*
      ((callee (LLVMGetNamedFunction module fname))
       ) ; TODO: eval/apply all args first: (args (map codegen args))
      ;if (callee == 0)
      ;return ErrorV("Unknown function referenced");
      ;If argument mismatch error.
      ;if (callee->arg_size() != Args.size())
      ;return ErrorV("Incorrect # arguments passed");
      (define call (LLVMBuildCall builder callee args "calltmp"))
      (LLVMSetInstructionCallConv call 'LLVMFastCallConv) ; MUST MATCH FUNCTION DEFINITION
      ; TODO: (LLVMSetTailCall call true)
      call ; Returned object can be used to reference the return value of this call
      ))

  #|
  // If F conflicted, there was already something named 'Name'.  If it has a
  // body, don't allow redefinition or reextern.
  if (F->getName() != Name) {
    // Delete the one we just made and get the existing one.
    F->eraseFromParent();
    F = TheModule->getFunction(Name);
    
    // If F already has a body, reject this.
    if (!F->empty()) {
      ErrorF("redefinition of function");
      return 0;
    }
    
    // If F took a different number of args, reject.
    if (F->arg_size() != Args.size()) {
      ErrorF("redefinition of function with different # args");
      return 0;
    }
  }
  
  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return 0;
}
  |#
  
  
    
  
  
  (define (gen-function ret-type func-name argtypes body)
    (let* (
      (numargs (length argtypes))
      (funtype (LLVMFunctionType ret-type argtypes #f)) ; #f = no varargs - they're incompatible w/ LLVMFastCallConv
      (func (LLVMAddFunction module func-name funtype))
	; TODO: clear env, or make new frame
      (args ; Unpack all arguments declared in func-type into a list:
       (for/list ((n (in-range numargs)))
	; TODO: add to env (append this whole list once it's done?)
        ;(LLVMSetValueName x "x") ; optional? hint for better names in IR?
        (LLVMGetParam func n)))
      )
    (LLVMSetFunctionCallConv func 'LLVMFastCallConv) ; CALLS MUST MATCH THIS
    ; TODO: (LLVMSetTailCall func true)
    (LLVMSetLinkage func 'LLVMPrivateLinkage)
    

      ; Tiger does it this way:
      #|
(let* ((fun (hash-ref all-functions name))
            (block (llvm-add-block-to-function fun)))
      (LLVMSetFunctionCallConv fun 'LLVMFastCallConv)
      (LLVMSetLinkage fun 'LLVMPrivateLinkage)
      (llvm-set-position block)
      (let* ((env (for/fold ((env global-environment)) ((arg-name arg-names) (i (in-naturals)))
                  (hash-set env arg-name (llvm-get-param (add1 i)))))
             (env (for/fold ((env env)) ((arg-name closed-names) (arg-type closed-types) (i (in-naturals)))
                   (hash-set env arg-name 
                    (llvm-int-to-ptr
                     (llvm-load (llvm-gep (llvm-get-param 0) 0 1 i))
                     (convert-type arg-type))))))
|#
      
      
    ; "A function definition contains a list of basic blocks, forming the CFG (Control Flow Graph) for the function.
    ; Each basic block may optionally start with a label (giving the basic block a symbol table entry), contains a list of instructions,
    ; and ends with a terminator instruction (such as a branch or function return).
    ; The first basic block in a function is special in two ways: it is immediately executed on entrance to the function, and it is not allowed to have predecessor basic blocks"

    ; TODO: generate prototype and check for errors/name colisions
    (define entry (LLVMAppendBasicBlockInContext context func "entry"))
    (LLVMPositionBuilderAtEnd builder entry)    ;place the builder
    (body args) ; Delegate the generation of the actual body code to the supplied function.
    (LLVMVerifyFunction func 'LLVMAbortProcessAction)
    ; TODO: (LLVMAddAlias module funtype func alias-name)
    func ; Return the function object.
  ))


#|  
  (define real-main
    (gen-function int-type "real-main" (list int-type int-type)
       (lambda (args) ;TODO: hardcoded. generate this dynamically
         ;body = return args[0]*args[1]+4
         (let* (
           (a (gen-primative '* (car args) (cadr args))) ; first arg times 2nd arg,
           (b (gen-primative '+ (gen-number 4) a)) ; plus four
           )
           (LLVMBuildRet builder b))) ; return it
    ))
 |#
 
  ;TODO: look at compile-expr in Tiger's code-gen.rkt
  ;TODO: better matching: http://docs.racket-lang.org/reference/match.html
  (define (walk-ast main-entry) ; main-entry is where we left off in the main
         (for/list ((node code-ast))
           (case (car node)
             ('define
               (let* (
                      (lhs (cadr node)) 
                      (rhs (caddr node))
                      (rhs-type (car rhs))
                      (rhs-val (cadr rhs)))
                 (case rhs-type
                   ('integer (begin (gen-number rhs-val)))
                   (else (debug "Define is Not an integer"))
                 )))
             ('procedure
              (let* (
                      (proc-name (cadr node))
                      (params (caddr node))
                      (body (cadddr node))
                      (name (symbol->string (gensym proc-name))) ; unique; TODO: is symbol->string un-uniquing these?
                      )
                (debug (string-append "Define " name))
                (gen-function int-type name (for/list ((n (in-range (length params)))) int-type)
                     (lambda (args)
                       (LLVMBuildRet builder (gen-number (length params))))) ))
             ('proc-apply
              (let* (
                      (name (cadr node))
                      (args+types (caddr node))
                      (proc (symbol->string name)) ; TODO: this will have to be manually looked up in the function table
                      (args (for/list ((arg+type args+types)) (gen-number (cadr arg+type))) ) ; just the arg; TODO: validate/dispatch-on type
                      )
                (debug (string-append "Apply " proc))
	      (LLVMPositionBuilderAtEnd builder main-entry)    ;place the builder
                (gen-call proc args) ; TODO: again, builder gets in wrong spot here...
                ;(LLVMBuildRet builder (gen-number (length params)))))
              ))
             (else (debug "Not a Define")) )) )
  
  ; Every program needs the standard function "int main(int,char**)"
  ; This is a driver/stub/wrapper that calls the real main when the compiled program is run.
  (define main
    (let* (
         (takes-varargs #f)
         (funtype (LLVMFunctionType int-type (list int-type (LLVMPointerType (LLVMPointerType (LLVMInt8Type) 0) 0) ) takes-varargs))
         (func (LLVMAddFunction module "main" funtype))
         (entry (LLVMAppendBasicBlockInContext context func "entry")) )
      
      (walk-ast entry)
      (LLVMPositionBuilderAtEnd builder entry) ; TODO: this is a hack. need to better keep track of the builder's place
      
      ;(define call-real-main (gen-call "real-main" (list (gen-number 5) (gen-number 7)) ))
      
(LLVMBuildRet builder (gen-number 0)) ; return 0 TODO: keep track of function calls and return the result of the last one.
;(LLVMBuildRet builder call-real-main) ; return real-main's return value

      (LLVMSetLinkage func 'LLVMExternalLinkage) ; main is the only External function - the others are Private
      func))
  
  
  
  
  
  
  
  
  (debug "Verifying generated code...")
  (let-values (((err) (LLVMVerifyModule module 'LLVMAbortProcessAction)))
    (when err
      (display err) (exit -1)))
  
  (debug "JIT compiling generated code...")
  (LLVMLinkInJIT) ; or (LLVMLinkInInterpreter)
  (LLVMDisposeBuilder builder)
  
  (debug (string-append "Writing file: " outfilename))
  (LLVMWriteBitcodeToFile module outfilename)
  
  (when verbose-mode (LLVMDumpModule module)) ; to stderr
  (llvm-dis outfilename) ; Since LLVMDumpModule only writes to stderr, instead get LLVM IR by disassembling the bitcode file.
    
  ; run inline:
  (cond (exec-mode
    (let (
          (engine (LLVMCreateExecutionEngineForModule module))
          (arg0 (LLVMCreateGenericValueOfInt int-type 5 #t))
          (arg1 (LLVMCreateGenericValueOfInt int-type 6 #t))
          )
      (let ((result (LLVMRunFunction engine main (list arg0 arg1))))
        (debug "Executing...")
        (LLVMGenericValueToInt result #t))))
    (else 0)) ; Return 0 for success.
  
  ) ; end code-gen
  
  
)
