(use extras) ;; for read-line
(use vector-lib) ;; for vector-map
(define flush-output-port flush-output)
(define-syntax import
  (syntax-rules ()
    ((import stuff ...)
     (begin) ;; do nothing
     )))
(define current-jiffy current-milliseconds)
(define (jiffies-per-second) 1000)
(define current-second current-seconds)
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square x) (* x x))
(define exact-integer? integer?)

;; bv2string
(use srfi-4 lolevel)

(define make-bytevector make-u8vector)
(define bytevector-u8-set! u8vector-set!)

(define (string->utf8 string)
  (let ((u8vector (make-u8vector (string-length string))))
    (move-memory! string u8vector)
    u8vector))

(define (utf8->string u8vector)
  (let ((string (make-string (u8vector-length u8vector))))
    (move-memory! u8vector string)
    string))

(define (this-scheme-implementation-name) (string-append "chickencsi-" (chicken-version)))
