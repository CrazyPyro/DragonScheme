(module ast racket
  ;;(require )
  
  ;AST helpers for the grammar
  (provide (all-defined-out))
  
(define (make-definition lhs rhs)
  (cons lhs rhs))
(define (make-identifier name)
  (list name))
(define (make-string name)
  (list name))
(define (make-integer name)
  (list name))
(define (make-procedure params body)
  (cons params body))
(define (make-procedure-application name args)
  (list 'procedure name args))

  )

