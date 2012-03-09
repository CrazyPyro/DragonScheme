#!/usr/bin/env racket

(module test-lexer racket
  (require rackunit
         parser-tools/lex ; Need some of the lexer tools for use in testing the lexer.
         "lexer.rkt" ; The lexer under test
         )
  
  
;; strip the source location from the position token
  ;; Taken from Racket's color-lexer.rkt
(define (strip-pos token)
   (match token
      [(position-token token start end) token]
      [else token]))
 
; Helper to create a unit test that compares the result of lexing a string of code, with the declared correct list of tokens that should be produced. 
(define (test-lexing codestring correct-tokens failmessage)
  (test-begin
   (for/or ((token (lex-all codestring))
              (correct-token (in-list correct-tokens))
              (tokencount (in-naturals))) ; count the number of tokens compared, for debugging
     (let ((token (strip-pos token))) ; Convert the position token to a plain one, for easier comparison here.
       (check-equal? (token-name token) correct-token
                     (string-append failmessage ": token " (number->string tokencount))) ; Display message, plus the ordinal of the mismatching token.
       (eq? token 'eof) )))) ; Stop comparing once an eof is seen - it's the first of infinitely many.
     
   
(define lexer-tests
  (test-suite
    "Tests for lexer.rkt"
    
    ;Simple Tests
    
    (test-lexing "" (list 'eof) "blank")
    (test-lexing "(" (list 'open-paren) "one open paren")
    (test-lexing ")" (list 'close-paren) "one close paren")
    (test-lexing "5" (list 'integer) "one integer")
    (test-lexing "+5" (list 'integer) "positive integer")
    (test-lexing "-5" (list 'integer) "negative integer")
    (test-lexing "; (5)" (list 'eof) "only a comment")
    (test-lexing "hello" (list 'identifier) "one identifier")
    (test-lexing "+" (list 'identifier) "identifier +")
    (test-lexing "-" (list 'identifier) "identifier -")
    (test-lexing "." (list 'identifier) "identifier .")
    (test-lexing "..." (list 'identifier) "identifier ...")
    (test-lexing "!$aSd%&*+-./:<=>?@^_42~" (list 'identifier) "identifier crazy")
    (test-lexing "  \"Hello, I am a string\"  " (list 'string) "one string")
    (test-lexing "cond" (list 'cond) "one cond")
    (test-lexing "define" (list 'define) "one define")
    (test-lexing "else" (list 'else) "one else")
    (test-lexing "lambda" (list 'lambda) "one lambda")
        
    ;(check-equal? (token-value (strip1 ((lex "5")))) 5 "integer value 5")
     
    ;Compound Tests
    
    (test-lexing "(define is-5?
                      (lambda(x)  ; this is a comment
                        (cond ((eq? x 5) \"yes!\") (else 0)))) ; this is another comment"
                  (list 'open-paren 'define 'identifier 'open-paren 'lambda 'open-paren 'identifier 'close-paren 'open-paren 'cond 'open-paren 'open-paren 'identifier 'identifier 'integer 'close-paren 'string 'close-paren 'open-paren 'else 'integer 'close-paren 'close-paren 'close-paren 'close-paren 'eof #t)
                  "Multiline. Every lexeme type plus eof and #t")
    
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren 'open-bracket 'close-bracket 'string)
                  "Should fail - missing last close paren")
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren)
                  "Should fail - got more than we were expecting")
    
    (test-lexing "(begin)"
      '(open-paren begin close-paren)
      "empty begin")
    (test-lexing "(begin 5 (\"test\"))"
      '(open-paren begin integer open-paren string close-paren close-paren)
      "simple begin")
    
    
  ))
    
 
  
(require rackunit/text-ui)
(run-tests lexer-tests)
  )
