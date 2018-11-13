(use-modules (ice-9 rdelim)
             (ice-9 syncase))
(define-syntax import
  (syntax-rules ()
    ((import stuff ...)
     (begin) ;; do nothing
     )))
(define flush-output-port force-output)
(define current-second current-time)
(define (jiffies-per-second) internal-time-units-per-second)
(define current-jiffy get-internal-real-time)
(define exact inexact->exact)
(define inexact exact->inexact)
(define (square x) (* x x))
(define (write-string str out) (display str out)) ; sufficient for tail.scm
(define (this-scheme-implementation-name) (string-append "guile-" (version)))
(cond
  ((string-ci> (version) "2.0")
   (read-enable 'r7rs-symbols)
   (print-enable 'r7rs-symbols)
   (use-modules (rnrs bytevectors)
                (rnrs base))))
(use-modules (srfi srfi-9))
