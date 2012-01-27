(module test-parser racket
  (require rackunit
         "parser.rkt" ; The parser under test
         )

(define lexer-tests
  (test-suite
    "Tests for parser.rkt"
    (check-not-equal? (parse "(define x 3)") '() "")
    (check-equal? (parse "(define is-5? 4)") (list 'is-5? 4) "Simple integer definition")
    (check-equal? (parse "(define is-5? x)") (list 'is-5? 'x) "Simple variable definition")
    (check-equal? (parse "(define is-5? \"yes!\")") (list 'is-5? "yes!") "Simple string definition")
    
    (check-equal? (parse "x") (list 'x) "Just an identifier")
    (check-equal? (parse "5") (list 5) "Just an integer")
    (check-equal? (parse "\"yes!\"") (list "yes!") "Just a string literal")
    
    
 ))
  
(require rackunit/text-ui)
(run-tests lexer-tests)
  )
