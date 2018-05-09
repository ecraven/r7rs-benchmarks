;; (use extras) ;; for read-line
;; (use vector-lib) ;; for vector-map
;; (define flush-output-port flush-output)
;; (define-syntax import
;;   (syntax-rules ()
;;     ((import stuff ...)
;;      (begin) ;; do nothing
;;      )))
;; (define current-jiffy current-milliseconds)
;; (define (jiffies-per-second) 1)
;; (define current-second current-seconds)
;; (define inexact exact->inexact)
;; (define exact inexact->exact)
;; (define (square x) (* x x))
;; (define exact-integer? integer?)

(define (this-scheme-implementation-name) (string-append "kawa-" (car (string-split (scheme-implementation-version) " "))))
