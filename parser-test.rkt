(module test-parser racket
  (require rackunit
         "parser.rkt" ; The parser under test
         )

(define lexer-tests
  (test-suite
    "Tests for parser.rkt"
    (check-not-equal? (parse "(define x 3)") '() "")
    (check-equal? (parse "(define is-5? 4)") (cons 'is-5? 4) "Simple integer definition")
    (check-equal? (parse "(define is-5? x)") (cons 'is-5? 'x) "Simple variable definition")
    (check-equal? (parse "(define is-5? \"yes!\")") (cons 'is-5? "yes!") "Simple string definition")
 ))
  
(require rackunit/text-ui)
(run-tests lexer-tests)
  )
