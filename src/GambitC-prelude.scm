(define (current-second) (truncate (current-jiffy)))
(define (jiffies-per-second) 1)
(define (current-jiffy) (time->seconds (current-time)))
(define flush-output-port force-output)
(define exact inexact->exact)
(define inexact exact->inexact)

(define (this-scheme-implementation-name) (string-append "gambitc-" (system-version-string)))
;; TODO: load syntax-case here, to get syntax-rules.
;; google says (load "~~/syntax-case"), but that doesn't work on my machine :-/

;; (define-syntax import
;;   (syntax-rules ()
;;     ((import stuff ...)
;;      (begin) ;; do nothing
;;      )))
