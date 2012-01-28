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
    
    ;(check-equal? (parse "(define is-5? (lambda 5))") '() "Lambda with syntax error")
    (check-equal? 
      (parse "(define is-5?
                      (lambda ()  ; this is a comment
                        \"five\"))
                        ; this is another comment")
      '(is-5? () "five")
      "Lambda with 0 params")
    (check-equal? 
      (parse "(define is-5?
                      (lambda (x)  ; this is a comment
                        \"five\"))")
      '(is-5? ((x)) "five")
      "Lambda with 1 param")
    (check-equal? 
      (parse "(define is-5? 
                      (lambda (x y z)  ; this is a comment
                        5))")
      '(is-5? ((x) (y) (z)) 5)
      "Lambda with 3 params")
    (check-equal? 
      (parse "(is-5? x y z)")
      '(procedure is-5? ((x) (y) (z)))
      "Procedure application of 3 args")
    (check-equal? 
      (parse "(is-5? x)")
      '(procedure is-5? ((x)))
      "Procedure application of 1 args")
    (check-equal? 
      (parse "(is-5?)")
      '(procedure is-5? ())
      "Procedure application of 0 args")
    (check-equal? 
      (parse "(define test (lambda (x y) (lambda( x) (lambda () 5))))")
      '(test ((x) (y)) ((x)) () 5) ;'(procedure is-5? ())
      "Nested procedure definitions")
    
    
    ;(((test 1 2) 3))
    
    
 ))
  
(require rackunit/text-ui)
(run-tests lexer-tests)
  )
