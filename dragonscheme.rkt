(module test-parser racket
  (require
         "parser.rkt" ; The parser
         "llvm.rkt" ; The code generator
         )
  ;
  (let ((path (sequence-ref (in-lines) 0)))
    (displayln (string-append "Parsing file: " path))
    ; Debugging:
    ;(let ((filestream (open-input-file path #:mode 'binary)))
    ;  (for ([line (in-lines filestream)])
    ;    (displayln line)))
    (let ((filestream (open-input-file path #:mode 'binary)))
      (parse filestream)) )
)