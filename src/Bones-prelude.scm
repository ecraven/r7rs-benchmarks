(define (this-scheme-implementation-name)
  (string-append "bones-" "unknown"))
(define (flush-output-port . a) a)
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square n) (* n n))
(define (jiffies-per-second)
  1000000000 ;; returns 1 on my Bones, which is wrong. this number should work for ?many? linuxen
  )
(define exact-integer? integer?)
;; vector-map
;; read-line
;; complex/rational functions
;; define-record-type
