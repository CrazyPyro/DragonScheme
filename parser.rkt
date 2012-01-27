(module parser racket
  (require racket/list ;for definition of "empty"
         racket/contract ; for provide/contract  TODO: not currently used
         parser-tools/yacc ; for parser. See docs: http://docs.racket-lang.org/parser-tools/Parsers.html
         parser-tools/lex ; Need some of the lexer tools for use in the parser. (position-line and position-col)
         "lexer.rkt" ; the lexer
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

  ; For external interface to this module:
  ; (provide/contract (parse (-> (or/c string? port?) expression?)))
 
  ; Shortcut instead of the above.  Just export everything;
  (provide (all-defined-out))
  (provide (for-syntax (all-defined-out))) ; need this too, to properly export macro-defined things

  
(define (parse p/s)
 (let ((port (if (string? p/s) (open-input-string p/s) p/s)))
  (port-count-lines! port)
  (lang-parser (lambda () (lang-lexer port)))))

;AST helpers for the grammar
  ;TODO: llvm code generation
(define (make-definition lhs rhs)
  (cons lhs rhs))
(define (make-identifier name)
  name)
(define (make-string name)
  name)
(define (make-integer name)
  name)

  
(define lang-parser
  ;A function which takes a function that produces tokens (the lexer function), and returns the parse tree created by the semantic actions.
  (parser
   
   (grammar 
    
    ; Each grammar-id is followed by list of productions, where each production is a pair 
    ; whose car can be a list of tokens and/or other grammar rules to match,
    ; and whose cdr is an action to take.
    
    
    (scheme
      ((definition) $1)
      ((expression) $1) )
    
    (definition ; The keyword "define" distinguishes definitions from expressions. 
      ((open-paren define identifier expression close-paren)
      (make-definition $3 $4)))
    
    ;The category of expressions consists of six alternatives: 
    ;variables, constants,
    ;primitive applications, (function) applications, and two varieties of conditionals.
    ;The last four are again composed of other expressions.
    ;The keyword cond distinguishes conditional expressions from primitive and function applications. 
    (expression
      ((identifier) (make-identifier $1))
      ((constant) $1)
      ;((expression expression) (cons $1 $2))
      )
    
    (constant
      ((string) (make-string $1))
      ((integer) (make-integer $1))
      )
    
    
    ) ; end of grammar
   
   ; (Optional) Precedence declarations to resolve shift/reduce and reduce/reduce conflicts as in yacc/bison.
   ;States with multiple shift/reduce or reduce/reduce conflicts (or some combination thereof) are not resolved with precedence.
   (precs 
    ;An assoc must be one of left, right or nonassoc.
    (left open-paren) 
    (nonassoc open-brace close-brace
              open-bracket close-bracket
              close-paren ) ; TODO: open-paren?  Pry not, since it is specified as left.
    ;(left else)
    ;(right assignment)
    ;(left or)
    ;(left and)
    ;(nonassoc comparison equal not-equal)
    ;(left plus)
    ;(left */)
    ;(nonassoc minus)    
    ;(right arrow)
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
