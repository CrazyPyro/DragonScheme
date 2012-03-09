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
                            "Specify the name of the output *.bc file. Default is [filename].bc An *.ll file will also be produced."
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
  
  (let ((path cmdline-args))
    (cond ((verbose-mode) (displayln (string-append "Reading file: " path))))
    (let ((filestream (open-input-file path #:mode 'binary)))
      (cond
        ((parse-only) (parse filestream))
        (else ; compile (and maybe exec)
         (let
            ((bcfilename
             (cond
               ((equal? (outfilename) "") ; no output file specified? Use input filename w/ extension changed to ".bc"
                (let*
                  ((basename path)
                   (extension (filename-extension path)) )
                  (string-append (substring basename 0 (- (string-length basename) (bytes-length extension))) "bc"))) ; replace extension with .bc
               (else (outfilename)))))
           ; The actual compile:
           (code-gen (parse filestream) bcfilename (verbose-mode) (exec-mode)))
         ))))
  
)
