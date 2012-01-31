#!/usr/bin/env racket

(module dragonscheme racket
  (require
         "lexer.rkt"
         "parser.rkt"
         "ast.rkt"
         "llvm.rkt"
         )
  ; The main user interface of the compiler.
  ; Open the filename passed as the (required) command-line argument, and parse it.

  
  (define verbose-mode (make-parameter #f))
  (define lex-only (make-parameter #f))
  (define parse-only (make-parameter #f))
  (define outfilename (make-parameter ""))
  (define exec-mode (make-parameter #f))
  
  (define cmdline-args
    (command-line  ; http://docs.racket-lang.org/reference/Command-Line_Parsing.html
     #:program "dragonscheme"
     #:once-each
     [("-v" "--verbose") "Compile with verbose messages."
                         (verbose-mode #t)]
     [("-o" "--output") output ; flag takes one argument
                            "Specify the name of the output file. If none is specified, none will be written."
                            (outfilename output)]
     #:once-any
     [("-p" "--parse") "Lex and parse only; Don't emit IR code."
                       (parse-only #t)]
     ;[("-l" "--lex") "Lex only; Don't parse or emit IR code."
     ;                (lex-only #t)]
     [("-x" "--exec") "Execute compiled IR code immediately."
                     (exec-mode #t)]
     #:multi
     ;
     #:args (filename) ; expect one command-line argument: <filename>
     ; return the argument as a filename to compile
     filename))
  
  (let ((path cmdline-args)) ;((sequence-ref (in-lines) 0)))
    (cond ((verbose-mode) (displayln (string-append "Reading file: " path))))
    (let ((filestream (open-input-file path #:mode 'binary))
          (outfilename (cond ((equal? (outfilename) "") (string-append path ".ll")) (else (outfilename)))))
      (cond
        ((parse-only) (parse filestream))
        (else (code-gen (parse filestream) outfilename (verbose-mode) (exec-mode)))
      )
    )
  )
  
)
