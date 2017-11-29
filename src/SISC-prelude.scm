(define-syntax import
  (syntax-rules ()
    ((import stuff ...)
     (begin) ;; do nothing
     )))
(define (jiffies-per-second) 1000)
(define current-jiffy system-time)
(define (current-second) (/ (current-jiffy) (jiffies-per-second)))
(define exact inexact->exact)
(define inexact exact->inexact)
(define (this-scheme-implementation-name)
  (string-append "SISC-" (getprop 'version (get-symbolic-environment '*sisc*))))
