(define-syntax import
  (syntax-rules ()
    ((import foo bar ...)
     #t)))

(define (this-scheme-implementation-name)
  "stklos")
