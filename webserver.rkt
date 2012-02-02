#!/usr/bin/env racket
(module dragonschemeweb racket
  (require
         web-server/servlet
         web-server/servlet-env
         "lexer.rkt"
         "parser.rkt"
         "ast.rkt"
         "llvm.rkt"
         )
; A web front-end to the compiler.
 
; A compilation is a (make-compilation src ir)
(struct compilation (src ast ir))

  ;A helper, to prevent complaints about the AST and IR not being xexprs.
 (define (to-string anything)
   (let ((o (open-output-string)))
     (write anything o)
     (get-output-string o)))

; start: request -> doesn't return
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (render-compiler-page
   (compilation "" "" "") ; Start with default/blank data
   request))
  
 ;Return value of name in bindings, or default if it is undefined.
 (define (extract-binding-with-default name bindings default)
    (with-handlers ([(lambda(e) #t) (lambda(e) default)])
           (extract-binding/single name bindings)))
  
; parse-compilation: bindings -> compilation
; Extracts a compilation out of the bindings.
(define (parse-compilation bindings)
  (let*
      ((src (extract-binding-with-default 'src bindings "(define empty 0)"))
      (ast (parse src))
      (ir (code-gen ast "" #t #t)))
    (compilation src ast ir)))

; render-compiler-page: Page request -> doesn't return
; Consumes a compiler struct and a request, and produces an HTML page
; of the content of the compiler.
(define (render-compiler-page a-compilation request)
  (local [(define (response-generator make-url)
            (response/xexpr
             `(html (head (title "DragonScheme web compiler"))
                    (body
                     (h1 "DragonScheme")
                     (form ((action
                             ,(make-url insert-compilation-handler)))
                           (textarea ((name "src") (rows "20") (cols "80")) "(define type-your scheme-here)" )
                           (input ((type "submit") (value "Compile")))
                           )
                     ,(render-compilation a-compilation) ;Display the results of compiling the user-supplied code.
                     ))))
 
          (define (insert-compilation-handler request)
            (render-compiler-page
             (parse-compilation (request-bindings request))
             request))]
 
    (send/suspend/dispatch response-generator)))
 
; render-compilation: compilation -> xexpr
; Consumes a compilation, produces an xexpr fragment of the compilation.
(define (render-compilation a-compilation)
  `(div ((class "compilation"))
        (h2 "Scheme source")
        (pre ((class "src"))
             ,(compilation-src a-compilation))
        (h2 "Parsed AST")
        (pre ((class "ast"))
             ,(to-string (compilation-ast a-compilation)))
        (h2 "(result returned by executing the) Generated LLVM IR")
        (pre ((class "ir"))
             ,(to-string (compilation-ir a-compilation)))
        ))
 
 (serve/servlet start
               #:stateless? #f ; #t doesn't work
               #:port 8080
               #:listen-ip #f
               #:servlet-regexp #rx"" ; #:servlet-path "/main"
               #:launch-browser? #f
               )
  
) ; end module

