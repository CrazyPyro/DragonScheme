(module test-lexer racket
  (require rackunit
         parser-tools/lex ; Need some of the lexer tools for use in testing the lexer.
         "lexer.rkt" ; The lexer under test
         racket/match ; For strip-pos function
         (for-syntax racket/base syntax/parse) ; 
         )
  
  
;; strip the source location from the position token
  ;; Taken from color-lexer.rkt
(define (strip-pos token)
   (match token
      [(position-token token start end) token]
      [else token]))
 
; Compare the result of lexing a string of code, with the declared correct list of tokens that should be produced. 
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
    
    (test-lexing "" (list 'eof) "blank")
    (test-lexing "(" (list 'open-paren) "one open paren")
    (test-lexing ")" (list 'close-paren) "one close paren")
    (test-lexing "5" (list 'integer) "one integer")
    (test-lexing "; (5)" (list 'eof) "only a comment")
    (test-lexing "hello" (list 'identifier) "one identifier")
    (test-lexing "  \"Hello, I am a string\"  " (list 'string) "one string")
    ;(check-equal? (token-value (strip1 ((lex "5")))) 5 "integer value 5")
     
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren 'open-bracket 'close-bracket 'string 'close-paren 'eof #t)
                  "Every lexeme type plus eof and #t")
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren 'open-bracket 'close-bracket 'string 'close-paren 'eof)
                  "Every lexeme type minus #t")
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren 'open-bracket 'close-bracket 'string 'close-paren)
                  "Every lexeme type minus eof and #t")
    
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren 'open-bracket 'close-bracket 'string)
                  "Should fail - missing last close paren")
    (test-lexing "(ident (5) [] \"str\" ) ; this is a comment"
                  (list 'open-paren 'identifier 'open-paren 'integer 'close-paren)
                  "Should fail - got more than we were expecting")
    
     
     
    ;(check-pred position-token? ((lex "")) "Lex empty list")
    ;(check-equal? (strip (lex "5"))
    ;           (list (make-token 5)))
    ;(check-equal? (strip (lex "4"))
    ;           (list (make-token 5)))
    ;;)
  ;(check-equal? (strip (lex-all "5"))
  ;              (list (token-integer 5)) "Lex 5")
  ;(check-equal? (strip (lex-all "5 8"))
  ;              (list (token-integer 5) (token-integer 8)) "Lex 5 8")
  
  ))
    
 
  
(require rackunit/text-ui)
(run-tests lexer-tests)
  )
