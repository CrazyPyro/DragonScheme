(module ast racket
  (require racket/list ;for definition of "empty"
         ;racket/contract ; for provide/contract
         )
  
  ;AST helpers for the grammar
  (provide (all-defined-out))
  

    
  ;NODE = (list 'node-type node-value-or-dummy-placeholder (list of child NODEs))
  
    (define (make-definition lhs rhs)
      (list 'define lhs (list rhs)))
    (define (make-identifier name)
      (list 'identifier name '()))
    (define (make-string str)
      (list 'string str '()))
    (define (make-integer val)
      (list 'integer val '()))
    (define (make-procedure params body)
      (list 'procedure 'procedure (cons body params)))
    (define (make-procedure-application name args)
      (list 'proc-apply name args))
    (define (make-cond cond-clauses)
      (list 'cond 'cond cond-clauses)) ; 2nd 'cond is unused placeholder.
    (define (make-cond-clause question answer)
      (list 'cond-clause 'cond-clause (list question answer)))
    ;TODO: unique nodes for question and answer
    (define (make-begin exps)
      (list 'compound-expression exps))
  
) ; end module
