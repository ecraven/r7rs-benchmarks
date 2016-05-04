;; (define-syntax import
;;   (syntax-rules ()
;;     ((import stuff ...)
;;      (begin) ;; do nothing
;;      )))
(define (current-second) (truncate (current-jiffy)))
(define (jiffies-per-second) 1)
(define (current-jiffy) (time->seconds (current-time)))
(define flush-output-port force-output)
(define exact inexact->exact)
(define inexact exact->inexact)

(define (this-scheme-implementation-name) (string-append "gambitc-" (system-version-string)))
