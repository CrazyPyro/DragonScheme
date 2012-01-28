(module code-gen racket
  (require "racket-llvm/llvm.rkt" ; racket-llvm library
         "parser.rkt" ; The parser
         )
  ;; racket-llvm/llvm.rkt depends on some  C++ bindings.
  ;; Copy/symlink racket-llvm.Makefile to racket-llvm/Makefile and run make.
  ;; (Otherwise it throws an error complaining "llvm-racket.so" can't be found.)
  ;; If you then get a "wrong ELF class" error, change -m32 to -m64 (or viceversa) in the Makefile and recompile.
  
  (display llvm-version-string)
)
