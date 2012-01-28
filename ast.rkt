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
  (list 'procedure params body))
(define (make-procedure-application name args)
  (list 'proc-apply name args))
(define (make-cond cond-clause cond-clauses)
  (list 'cond cond-clause cond-clauses))
(define (make-cond-clause question answer)
  (list question answer))

  )
