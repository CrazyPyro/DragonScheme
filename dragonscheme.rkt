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
  (define raw-code (make-parameter #f))
  
  (define cmdline-args
    (command-line  ; http://docs.racket-lang.org/reference/Command-Line_Parsing.html
     #:program "dragonscheme"
     #:once-each
     [("-v" "--verbose") "Compile with verbose messages."
                         (verbose-mode #t)]
     [("-o" "--output") output ; flag takes one argument
                            "Specify the name of the output *.bc file. Default is [filename].bc An *.ll file will also be produced."
                            (outfilename output)]
     [("-i") "Instead of an input filename, take raw code to compile.  Must also specify either -p or -o"
                            (raw-code #t)]
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
  
  (define (web-pretty-print xexp)
  (let ((pretty-xexp (open-output-string)))
    (pretty-print xexp pretty-xexp)
    (get-output-string pretty-xexp)))
  
  (let ((path cmdline-args))
    (let ((filestream (cond
                        ((raw-code) (open-input-string path)) ; "path" is the actual code
                        (else (open-input-file path #:mode 'binary))))) ; otherwise it's the path to the code file
      (cond
        ((parse-only) (parse filestream))
        (else ; compile (and maybe exec)
         (let
            ((bcfilename
             (cond
               ((equal? (outfilename) "") ; no output file specified? Use input filename w/ extension changed to ".bc"
                ;TODO: barf if raw-code is true
                (let*
                  ((basename path)
                   (extension (filename-extension path)) )
                  (string-append (substring basename 0 (- (string-length basename) (bytes-length extension))) "bc"))) ; replace extension with .bc
               (else (outfilename)))))
           ; The actual compile:
           (code-gen (parse filestream) bcfilename (verbose-mode) (exec-mode)))
         ))))
  
)
