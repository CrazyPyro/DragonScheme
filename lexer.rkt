(module lexer racket
  
  (require parser-tools/lex
         racket/list ;for definition of "empty" - used once.
         racket/contract ; for provide/contract
         (prefix-in : parser-tools/lex-sre)) ;pre-defined abbreviations for lexer regexs (all prefixed with a : per Racket docs)

; These are things we need to process further to get the "value" of once we recognize them:
(define-tokens lang-tokens (integer identifier string))

; These don't have an internal "value" that need to be lexed - just recognize when they occur. (Basically, keywords)
(define-empty-tokens lang-empty-tokens 
  (
   ; Tokens for Scheme special characters:
   eof open-paren close-paren open-bracket close-bracket open-brace close-brace pipe
   ;TODO: period (for dot-notation) elipsis (... notating varargs)
   ; Tokens for core Scheme keywords:
   cond define else lambda
   ;TODO: 'quote #t #f
   ;TODO: and begin car case cons cdr  delay display do  if  let let* letrec or 
   ;TODO:  map, set, quasiquote, unquote, unquote-splicing
   ;TODO: Rest of R5RS: cadr (and variants), call-with-current-continuation call-with-input-file call-with-output-file call-with-values ceiling, char (all variants), define-syntax dynamic-wind for-each let-syntax letrec-syntax syntax-rules
   ))

  ; For external interface to this module:
  (provide/contract ; These take a string, or a string opened as a port:
   (lex (-> (or/c string? port?) (-> position-token?))) ;procedure?  This is used by the parser.
   (lex-all (-> (or/c string? port?) sequence?)) ) ; Similar, but for use by the unit tests.
  (provide lang-tokens lang-empty-tokens) ; The parser also needs to know about all of the lexer's tokens.
  
  
  (define (lex p/s)
    (let ((port (if (string? p/s) (open-input-string p/s) p/s))) ; If string, convert to port.
      (port-count-lines! port)
      (lambda () (lang-lexer port))
      ))
  
  ;Returns a sequence (of tokens) whose elements are produced by calling lang-lexer on port.
  ;Note that once it runs out of characters to lex, or will return 'eof ad infinum, so caller needs to watch for this.
  (define (lex-all p/s)
    (let ((port (if (string? p/s) (open-input-string p/s) p/s))) ; If string, convert to port.
      (port-count-lines! port)
      (in-port (lambda (p) (lang-lexer p)) port) ;TODO: (lambda () (lang-lexer p)) -> (lex p) ; (lang-lexer p) -> ((lex p))
      ))
  
  
; Some abbreviations for convenience:
(define-lex-abbrevs
  (digit (:/ #\0 #\9)) ; 0-9
  (number (::
           ;(:? "+-") ; TODO: Include " . + - " per R5RS 2.3
           (:+ digit)))
  
  ; Per R5RS 2.1, an identifier starts with a letter or one of these "extended alphabetic characters."
  (intentifier-inital (:+ alphabetic #\_ #\< #\> #\/ #\? #\! #\* #\= #\$ #\% #\& #\: #\@ #\^ #\~ ))
  ; The following keyboard non-alphanumerics are not included '`,\"()[]{};|
  ;  because they already have special meaning, respectively: quote, quasiquote, unquote, string-literal, 3 ways of delimiting expressions, comment, and reserved.
  ;  And any following characers can also be number characters ( + - . digit)
  (identifier-full (:or
                    (:: intentifier-inital (:* intentifier-inital digit #\+ #\- #\. )) ; general case
                    ; Special case to above, these are valid identifiers: ... + . -
                    ;(:= 1 #\+) (:= 1 #\-) ; TODO: + and - here are clashing with those in 'number
                    (:= 1 #\.) (:= 3 #\.) ; TODO: These might also clash, once decimals are supported in 'number
                    ))
  
  ; Per R5RS 2.2:
  (any-whitespace (:+ whitespace)) ; All whitespace is identical, no matter how much of it there is. ;TODO was *
  (comment (:: ";" ; comments start with ;
                    (:* (:~ "\n")) ; and go to end of line
                    ;; Chomp the newline at the end, if it's there.  (Last line of the file could be a comment without a newline)
                    (:? "\n" "\r")))
)


; String lexer is used by the main lexer to lex out quoted string-literals.
  ;; Taken verbatim from endobson's "Tiger" lexer
(define (string-lexer port start-pos) ; TODO: (define string-lexer (lexer-src-pos
  (define (digit? x)
    (and (memq x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t))  ; TODO: vs digit abbrv above?

  (define (loop next-char chars)
    (define (continue char)
      (loop (read-char port) (cons char chars)))

    (case next-char
      ((#\") ; quoted string literals
       (let-values (((line column pos) (port-next-location port)))
         (make-position-token
          (token-string (list->string (reverse chars)))
          start-pos
          (make-position pos line column))))
      ((#\\) ; special backslash-escaped chars
       (let ((char (read-char port)))
         (cond 
           ((memq char '(#\" #\\)) (continue char)) ; Double-backslash is literal backslash
           ((equal? char #\t) (continue #\tab))
           ((equal? char #\n) (continue #\newline))
           ((digit? char)
            (let ((char2 (read-char port)) (char3 (read-char port)))
              (if (and (digit? char2) (digit? char3))
                  (continue (integer->char (string->number (string char char2 char3))))
                  (error 'string-lexer "Bad digit escape sequence"))))
           ((equal? char #\^) ; TODO: iso-control?
            (error 'string-lexer "Control escapes not supported yet"))
           ((char-whitespace? char)
            (let whitespace-loop ((char (read-char port)))
              (cond
                ((char-whitespace? char) (whitespace-loop (read-char port)))
                ((equal? char #\\) (loop (read-char port) chars))
                (else (error 'string-lexer "Bad character ~a in formatting escape" char)))))
           (else (error 'string-lexer "Unknown escape character ~a" char))))) ; end of special backslash-escaped chars, and case
      (else (loop (read-char port) (cons next-char chars))))) ; default case, and end of loop definition
  
  (loop (read-char port) empty)) ; end of string-lexer
 

(define lang-lexer
  (lexer-src-pos ; Pairs of ("string seen in input" token-to-return)
   
   ; The very core stuff:
   ((eof) (token-eof)) ; End-of-file
   (comment (return-without-pos (lang-lexer input-port))) ; Skip over comments
   ((:? "\n" "\r") (return-without-pos (lang-lexer input-port))) ; Skip over newlines
   (any-whitespace (return-without-pos (lang-lexer input-port))) ; Skip over any other whitespace 
   
   ("(" (token-open-paren))
   (")" (token-close-paren))
   ; Reserved for future use:
   ("[" (token-open-bracket))
   ("]" (token-close-bracket))
   ("{" (token-open-brace))
   ("}" (token-close-brace))
   ("|" (token-pipe))
  
   ; Constants/literals:
   (number (token-integer (string->number lexeme)))
   ; See a quote?  Call the helper to lex out the string literal:
   ("\"" (return-without-pos (string-lexer input-port start-pos)))
   
   
   ; core Scheme keywords:
   ("cond" (token-cond))
   ("define" (token-define))
   ("else" (token-else))
   ("lambda" (token-lambda))
   ;TODO: handle more
   ;("let" (token-let))
   
   ; Anything that looks like an identifier AND is not one of the previous keywords, is an identifier.
   (identifier-full (token-identifier (string->symbol lexeme)))

   
   (any-char (error (string-append "Unknown token: " lexeme)))  ; This prevents "Warning: lexer can accept the empty string."
   
   )) ; end of lexer

) ; end of module
