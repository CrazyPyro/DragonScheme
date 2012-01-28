(module parser racket
  (require
         racket/contract ; for provide/contract  TODO: not currently used
         parser-tools/yacc ; for parser. See docs: http://docs.racket-lang.org/parser-tools/Parsers.html
         parser-tools/lex ; Need some of the lexer tools for use in the parser. (position-line and position-col)
         "lexer.rkt" ; the lexer
         "ast.rkt" ;AST helpers for the grammar
         )
; Note from Racket docs:
  ;Each time the Racket code for a parser is compiled (e.g. when a ".rkt" file containing a parser form is loaded), the parser generator is run.
  ;To avoid this overhead place the parser into a module and compile the module to a ".zo" bytecode file.
  
; See also: R5RS Chapter 7 - Formal syntax and semantics
; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_chap_7
;http://ragnermagalhaes.blogspot.com/2007/08/bison-lisp-grammar.html
  ;http://stackoverflow.com/questions/3146771/building-lisp-scheme-like-parse-tree-with-flex-bison
  ;http://stackoverflow.com/questions/517113/lisp-grammar-in-yacc
  ;http://www.antlr.org/wiki/display/ANTLR3/Quick+Starter+on+Parser+Grammars+-+No+Past+Experience+Required
  ;http://docs.racket-lang.org/reference/syntax-model.html#%28part._expansion%29
  
  ; For external interface to this module:
  (provide/contract (parse (-> (or/c string? port?) (or/c list? pair?)))) ; TODO: replace list? with custom AST type
 
  ; Shortcut instead of the above.  Just export everything;
  ;(provide (all-defined-out))
  ;(provide (for-syntax (all-defined-out))) ; need this too, to properly export macro-defined things

  
(define (parse p/s)
 (let ((port (if (string? p/s) (open-input-string p/s) p/s))) ; If string, convert to port.
  (port-count-lines! port)
  (lang-parser (lex port) )))

  
(define lang-parser
  ;A function which takes a function that produces tokens (the lexer function), and returns the parse tree created by the semantic actions.
  (parser
   
   (grammar 
    
    ; Each grammar-id is followed by list of productions, where each production is a pair 
    ; whose car can be a list of tokens and/or other grammar rules to match,
    ; and whose cdr is an action to take.
    
    
    (scheme ; The main non-terminal: Scheme is composed of definitions and expressions.
      ((definition) $1)
      ((expression) $1) )
    
    (definition ; The keyword "define" distinguishes definitions from expressions. 
      ((open-paren define identifier expression close-paren)
      (make-definition $3 $4)))
    
    ;The category of expressions consists of six alternatives: 
    ;variables (identifiers), constants, (function) applications,
    ;primitive applications, and two varieties of conditionals.
    ;The last four are again composed of other expressions.
    ;The keyword cond distinguishes conditional expressions from primitive and function applications. 
    (expression
      ((identifier) (make-identifier $1))
      ((constant) $1)
      ((open-paren lambda open-paren param-list close-paren expression close-paren) (make-procedure $4 $6))
      ;TODO: procedure, not just identifier - could apply a lambda.
      ; TODO: Can't nest applications: ((
      ((open-paren identifier arg-list close-paren) (make-procedure-application $2 $3))
      ((open-paren cond cond-clause cond-clauses close-paren) (make-cond $3 $4))
      )
    
    (constant
      ((string) (make-string $1))
      ((integer) (make-integer $1))
      )
    
    (param-list ; A list of 0 or more identifiers
      (() '()) ; empty param list
      ((identifier param-list) (cons (make-identifier $1) $2))
      )
    
    (arg-list ; A list of 0 or more arguments to which to apply a procedure
      (() '()) ; empty arg list
      ((identifier arg-list) (cons (make-identifier $1) $2))
      ((constant arg-list) (cons (make-identifier $1) $2)) ; TODO: shouldn't be make-identifier
      )
    
    (cond-clauses ; A list of 1 or more question.answer clauses
      ((cond-clause) $1)
      ((cond-clause cond-clauses) (list $1 $2))
      )
    (cond-clause
     ((open-paren expression expression close-paren) (make-cond-clause $2 $3))
     )
     
    
    ) ; end of grammar
   
   ; (Optional) Precedence declarations to resolve shift/reduce and reduce/reduce conflicts as in yacc/bison.
   ;States with multiple shift/reduce or reduce/reduce conflicts (or some combination thereof) are not resolved with precedence.
   (precs 
    ;An assoc must be one of left, right or nonassoc.
    (left open-paren open-brace open-bracket) 
    (nonassoc close-brace
              close-bracket
              close-paren )
    )
   
   (tokens lang-tokens lang-empty-tokens) ; Make all of the tokens defined in lexer.rkt available to the parser.
   (start scheme) ; A list of starting non-terminals for the grammar. A separate parse function is returned for each, in a list.
                ; In this case, just expr - so a single parse function.
   (src-pos) ; Tell the parser to expect the format produced by a (lexer-src-pos) rather than a plain (lexer)
   (error ; Specify a parser error handler function - to be called if the parser encounters an error.
    (lambda (tok-ok? tok-name tok-value start-pos end-pos) ; last 2 parameters only if (src-pos) defined
     ; Try to use error productions to continue parsing, if recoverable. Otherwise it raises exn:fail:read.
     (if tok-ok?
      (error 'parser "Got unexpected token ~a(~a) at ~a:~a-~a:~a"
       tok-name tok-value
       (position-line start-pos)
       (position-col  start-pos)
       (position-line end-pos)
       (position-col  end-pos))
      (error 'parser "Bad Token at ~a:~a-~a:~a"
       (position-line start-pos)
       (position-col  start-pos)
       (position-line end-pos)
       (position-col  end-pos)))))
   (debug "parser.debug.txt") ;Optional debugging output file 
   (end eof))) ;parsing ends when the eof token is encountered.
  ;End of parser


  )
