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
  
  ; Type abbreviation
  (define int-type (LLVMInt32TypeInContext context))
  
  ; All numbers are int32's for now
  (define (gen-number n)
    (LLVMConstInt (LLVMInt32TypeInContext context) n false))
  
  ; Variable reference
  (define (gen-var name)
    (let ((var (hash-ref env name))) ; TODO: custom error as 3rd arg
          var)) ; TODO: add cutom type-tag to differentiate between Value and Function? May not have to: instead: TheModule->getFunction(Callee)
  
  ; Variable definition
  (define (gen-def name val)
    (hash-set! env name val))
  
  (define (gen-primative op L R)
    ;TODO: eval/apply L and R first?
    (cond 
      ((eq? op '+) (LLVMBuildAdd builder L R "addtmp"))
      ((eq? op '-) (LLVMBuildSub builder L R "subtmp"))
      ((eq? op '*) (LLVMBuildMul builder L R "multmp"))
      ((eq? op '/) (LLVMBuildSDiv builder L R "divtmp"))
      ((eq? op '%) (LLVMBuildSRem builder L R "modtmp"))
      ((eq? op '&) (LLVMBuildAnd builder L R "andtmp"))
      ((eq? op 'or) (LLVMBuildOr builder L R "ortmp"))
      ((eq? op '^) (LLVMBuildXor builder L R "xortmp"))
      ((eq? op '~) (LLVMBuildNot builder L R "nottmp"))
      ((eq? op '<<) (LLVMBuildShl builder L R "shltmp"))
      ((eq? op '>>) (LLVMBuildAShr builder L R "shrtmp")) ;TODO: or LShr?
      ((eq? op '=) (LLVMBuildICmp builder 'LLVMIntEQ L R "cmptmp"))
      ((eq? op '<>) (LLVMBuildICmp builder 'LLVMIntNE L R "cmptmp"))
      ((eq? op '<) (LLVMBuildICmp builder 'LLVMIntSLT L R "cmptmp"))
      ((eq? op '<=) (LLVMBuildICmp builder 'LLVMIntSLE L R "cmptmp"))
      ((eq? op '>) (LLVMBuildICmp builder 'LLVMIntSGT L R "cmptmp"))
      ((eq? op '>=) (LLVMBuildICmp builder 'LLVMIntSGE L R "cmptmp"))
      (else error "invalid primative")))
  
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
      (LLVMBuildCall builder callee args "calltmp")))

  #|
Function *PrototypeAST::Codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type*> Doubles(Args.size(),
                             Type::getDoubleTy(getGlobalContext()));
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()),
                                       Doubles, false);
  
  Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);
  
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
  
  // Set names for all arguments.
  unsigned Idx = 0;
  for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size();
       ++AI, ++Idx) {
    AI->setName(Args[Idx]);
    
    // Add arguments to variable symbol table.
    NamedValues[Args[Idx]] = AI;
  }
  
  return F;
}
|#
 #|
Function *FunctionAST::Codegen() {
  NamedValues.clear();
  
  Function *TheFunction = Proto->Codegen();
  if (TheFunction == 0)
    return 0;
  
  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
  Builder.SetInsertPoint(BB);
  
  if (Value *RetVal = Body->Codegen()) {
    // Finish off the function.
    Builder.CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    return TheFunction;
  }
  
  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return 0;
}
  |#
  
  ;TODO: hardcoded. generate this dynamically
  (define (gen-body args)
    ;body = return args[0]*2+3
    (let* (
           (a (gen-primative '* (car args) (cadr args))) ; first arg times 2nd arg,
           (b (gen-primative '+ (gen-number 3) a)) ; plus three
           )
      (LLVMBuildRet builder b))) ; return it
    
  (define (gen-function ret-type func-name argtypes body)
    (let* (
      (numargs (length argtypes))
      (funtype (LLVMFunctionType ret-type argtypes numargs))
      (func (LLVMAddFunction module func-name funtype))
      (args ; Unpack all arguments declared in func-type into a list:
       (for/list ((n (in-range numargs)))
        ;(LLVMSetValueName x "x") ; optional hint for better names in IR
        (LLVMGetParam func n)))
      )
      
    ; a block:
    (define entry (LLVMAppendBasicBlockInContext context func "entry"))
    ;place the builder
    (LLVMPositionBuilderAtEnd builder entry)
    
    (body args) ; Delegate the generation of the actual body code to the supplied function.
    
    
    func)) ; experimental: return func object to caller
  
  
  ; Every program needs a function called "main" ;TODO: This isn't magic; still pry needs some linkage to be a runnable stand-alone program.
  (define main
    (gen-function int-type "main" (list int-type int-type) gen-body))
  
  
  
  
  
  
  
  
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
          (arg0 (LLVMCreateGenericValueOfInt int-type 5 #t))
          (arg1 (LLVMCreateGenericValueOfInt int-type 6 #t))
          )
      (let ((result (LLVMRunFunction engine main (list arg0 arg1))))
        (debug "Executing...")
        (LLVMGenericValueToInt result #t))))
    (else 0)) ; Return 0 for success.
  
  ) ; end code-gen
  
  
)
