(module test-parser racket
  (require rackunit
         "parser.rkt" ; The parser under test
         )

(define lexer-tests
  (test-suite
    "Tests for parser.rkt"
    
    (check-equal? (parse "(define is-5? 4)") '(define is-5? ((integer 4 ()))) "Simple integer definition")
    (check-equal? (parse "(define is-5? x)") '(define is-5? ((identifier x ()))) "Simple variable definition")
    (check-equal? (parse "(define is-5? \"yes!\")") '(define is-5? ((string "yes!" ()))) "Simple string definition")
    
    (check-equal? (parse "x") '(identifier x ()) "Just an identifier")
    (check-equal? (parse "5") '(integer 5 ()) "Just an integer")
    (check-equal? (parse "\"yes!\"") '(string "yes!" ()) "Just a string literal")
    
    ;(check-exn exn:fail? (parse "(define is-5? (lambda 5))") "Lambda with syntax error")
    (check-equal? 
      (parse "(define is-5?
                      (lambda ()  ; this is a comment
                        \"five\"))
                        ; this is another comment")
      '(define is-5? ((procedure procedure ((string "five" ())))))
      "Lambda with 0 params")
    (check-equal? 
      (parse "(define is-5?
                      (lambda (x)  ; this is a comment
                        \"five\"))")
      '(define is-5? ((procedure procedure ((string "five" ()) x))))
      "Lambda with 1 param")
    (check-equal? 
      (parse "(define is-5? 
                      (lambda (x y z)  ; this is a comment
                        5))")
      '(define is-5? ((procedure procedure ((integer 5 ()) x y z))))
      "Lambda with 3 params")
    (check-equal? 
      (parse "(is-5? x y z)")
      '(proc-apply is-5? ((identifier x ()) (identifier y ()) (identifier z ())))
      "Procedure application of 3 args")
    (check-equal? 
      (parse "(is-5? x)")
      '(proc-apply is-5? ((identifier x ())))
      "Procedure application of 1 args")
    (check-equal? 
      (parse "(is-5?)")
      '(proc-apply is-5? ())
      "Procedure application of 0 args")
    
    (check-equal? 
      (parse "(define test (lambda (x y) (lambda( x) (lambda () 5))))")
      '(define test ((procedure procedure ((procedure procedure ((procedure procedure ((integer 5 ()))) x)) x y))))
      "Nested procedure definitions")
    #|
    (check-equal? 
      (parse "(((test 1 2) 3))")
      '(proc-apply)
      "Nested procedure applications of 2, 1, and 0 args.")
    |#
    (check-equal? 
      (parse "(cond  )")
      '(cond cond ())
      "Cond empty")
    
    (check-equal? 
      (parse "(cond (x 0) (y 1) )")
      '(cond
         cond
         ((cond-clause cond-clause ((identifier x ()) (integer 0 ())))
          (cond-clause cond-clause ((identifier y ()) (integer 1 ())))))
      "Cond minimal")
    
    (check-equal? 
      (parse "(cond (x 0) (y 1) (z 2) (w 3) )")
      '(cond
         cond
         ((cond-clause cond-clause ((identifier x ()) (integer 0 ())))
          (cond-clause cond-clause ((identifier y ()) (integer 1 ())))
          (cond-clause cond-clause ((identifier z ()) (integer 2 ())))
          (cond-clause cond-clause ((identifier w ()) (integer 3 ())))))
      "Cond extended")
    
    (check-equal? 
      (parse "(define test (lambda (x y) (lambda(x) (cond ((= x 0) \"zero\") ((= x 1) 1) ((= x 2) y)))))")
      '(define test
         ((procedure
           procedure
           ((procedure
             procedure
             ((cond
                cond
                ((cond-clause cond-clause ((proc-apply = ((identifier x ()) (integer 0 ()))) (string "zero" ())))
                 (cond-clause cond-clause ((proc-apply = ((identifier x ()) (integer 1 ()))) (integer 1 ())))
                 (cond-clause cond-clause ((proc-apply = ((identifier x ()) (integer 2 ()))) (identifier y ())))))
              x))
            x
            y))))
      "Proc-apply within Cond within nested procedures")
    
 ))
  
(require rackunit/text-ui)
(run-tests lexer-tests)
  )
