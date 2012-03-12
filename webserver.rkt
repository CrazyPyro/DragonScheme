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
; Must be launched from within DragonScheme dir, due to expectations/assumptions about relative paths.
  ;TODO:  To enforce this, require webserver.rkt detedted in (find-system-path 'orig-dir)
  
(define dragonscheme-url "/dragonscheme") ; The relative URL to access this servlet
(define tmpdir (build-path (find-system-path 'orig-dir) "tmp")) ; where to put compiler output files
 
; A compilation is a (make-compilation src ir)
(struct compilation (src ast ir outfile))

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
   (compilation "" "" "" "") ; Start with default/blank data
   request))
  
 ;Return value of name in bindings, or default if it is undefined.
 (define (extract-binding-with-default name bindings default)
    (with-handlers ([(lambda(e) #t) (lambda(e) default)])
           (extract-binding/single name bindings)))
  
; parse-compilation: bindings -> compilation
; Extracts a compilation out of the bindings.
  ; This is where the actual compiler routines are called.
(define (parse-compilation bindings)
  (let*
      ((src (extract-binding-with-default 'src bindings "(define empty 0)"))
      (ast (parse src))
      (outfile (make-temporary-file "web~a.bc" #f tmpdir))
      (ir (code-gen ast outfile #t #t)))
    (compilation src ast ir outfile)))

; render-compiler-page: Page request -> doesn't return
; Consumes a compiler struct and a request, and produces an HTML page
; of the content of the compiler.
(define (render-compiler-page a-compilation request)
  (local [(define (response-generator make-url)
            (response/xexpr
             `(html (head
			(title "DragonScheme web compiler")
                   (link ((rel "stylesheet")
				(href "/style.css")
				(type "text/css")
                           (title "light")))
			(link ((rel "alternate stylesheet")
				(href "/dark.css")
				(type "text/css")
                           (title "dark"))))
                    (body
                     (h1 "DragonScheme")
                     (h2 "Web Demo")
                     (h3 ,(string-append "(LLVM version on server: " (version) ")" ))
                     (p (a ((href ,dragonscheme-url)) "Click here to reset page if you get a \"page expired\" error."))
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

; Generate links for the downloadable .bc and .ll files
(define (download-links a-compilation)
  (let ((outfile (compilation-outfile a-compilation)))
    (cond
      ((path? outfile)
       (let* (
            (bc-file (path->string (file-name-from-path outfile)))
            (ll-file (string-append (substring bc-file 0 (- (string-length bc-file) 3)) ".ll")) ; replace .bc extension with .ll
            )
         `(p
           (h2 "Generated LLVM IR:")
           (pre (code ((class "language-llvm") (id "ir"))
              ,(call-with-input-file (build-path tmpdir ll-file)
                  (lambda (infile)
                    ; TODO: 'return-linefeed is a *nix-only hack to read the whole file as one line, since the file won't contain a 'return-linefeed on *nix.
                    (read-line infile 'return-linefeed)))))
           (h2 "Downloads:")
           (ul (li (a ((href ,bc-file)) "Generated LLVM bitcode file") " (binary, execute using " (kbd "lli " (samp ,bc-file) "; echo $?") " )")
             (li (a ((href ,ll-file)) "Disassembled LLVM IR file (human-readable)")))
           )))
      (else "")))) ; No output file specified? Then nothing to show.
  

(define (web-pretty-print xexp)
  (let ((pretty-xexp (open-output-string)))
    (pretty-print xexp pretty-xexp)
    (get-output-string pretty-xexp)))
  
; render-compilation: compilation -> xexpr
; Consumes a compilation, produces an xexpr fragment of the compilation.
(define (render-compilation a-compilation)
  `(div ((class "compilation"))
        (h2 "Original Source Code:")
        (pre (code ((class "language-scheme") (id "src"))
             ,(compilation-src a-compilation)))
        (h2 "Parsed AST:")
        (pre (code ((class "language-scheme") (id "ast"))
             ,(web-pretty-print (compilation-ast a-compilation)) ))
        (h2 "Execution Result:")
        (pre (samp ((id "output"))
             ,(to-string (compilation-ir a-compilation))))
        (p ,(download-links a-compilation))
        ))
 

 (serve/servlet start
               #:stateless? #f ; #t doesn't work
               #:port 8080
               #:listen-ip #f
	#:servlet-path dragonscheme-url
               #:launch-browser? #f
	#:extra-files-paths (list (build-path "htdocs") tmpdir)
               )
  
) ; end module

