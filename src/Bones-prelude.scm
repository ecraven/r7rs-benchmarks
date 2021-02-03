(define (this-scheme-implementation-name)
  (string-append "bones-" "8")) ;; hard-coded for last release, doesn't seem to be included in bones...
(define (flush-output-port . a) a)
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square n) (* n n))
(define (jiffies-per-second)
  1000000000 ;; returns 1 on my Bones, which is wrong. this number should work for ?many? linuxen
  )
(define exact-integer? integer?)
(define (integer-length n)
  (inexact->exact (ceiling (/ (log n)
                              (log 2)))))
;; taken from mit-scheme, src/runtime/primitive-arithmetic.scm
(define (exact-integer-sqrt n)
  (if (= 0 n)
      (values 0 0)
      (let loop
	  ((i
	    (expt 2
		  (let ((n-bits (integer-length n)))
		    (if (= 0 (remainder n-bits 2))
			(quotient n-bits 2)
			(+ (quotient n-bits 2) 1))))))
	(let ((j (quotient (+ i (quotient n i)) 2)))
	  (if (>= j i)
	      (values i (- n (* i i)))
	      (loop j))))))
;; vector-map
;; read-line
;; complex/rational functions
;; define-record-type
