(module ast racket
  (require racket/list ;for definition of "empty"
     ;racket/contract ; for provide/contract
     )
  
  ;AST helpers for the grammar
  (provide (all-defined-out))
    
  ; Type tagging per http://mitpress.mit.edu/sicp/full-text/sicp/book/node44.html
  
    (define (make-definition lhs rhs)
      (list 'define lhs rhs))
    (define (make-identifier name)
      (list 'identifier name))
    (define (make-string str)
      (list 'string str))
    (define (make-integer val)
      (list 'integer val))
    (define (make-lambda-procedure params body)
      (list 'procedure 'lambda params body))
    (define (make-define-procedure name params body)
      (list 'procedure name params body))
    (define (make-procedure-application name args)
      (list 'proc-apply name args))
    (define (make-cond cond-clauses)
      (list 'cond cond-clauses))
    (define (make-cond-clause question answer)
      (list 'cond-clause (list 'cond-question question) (list 'cond-answer answer)) )
    (define (make-cond-clause-else answer)
      (list 'cond-clause (list 'cond-question-else) (list 'cond-answer answer)) ) 
    (define (make-begin exps)
      (list 'compound-expression exps))
  

) ; end module
