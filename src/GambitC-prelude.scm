(declare (standard-bindings)   ;; builtin functions like + will not be redefined
         (extended-bindings)   ;; Gambit functions like fixnum? will not be redefined
         (block)               ;; user-defined functions not set! in the file will not be redefined
         (not safe))

(define (current-second) (truncate (current-jiffy)))
(define (jiffies-per-second) 1)
(define (current-jiffy) (time->seconds (current-time)))
(define flush-output-port force-output)
(define exact inexact->exact)
(define inexact exact->inexact)
(define (exact-integer? x) (and (exact? x) (integer? x)))
;; thanks to gambiteer https://github.com/ecraven/r7rs-benchmarks/issues/20
(define (exact-integer-sqrt y)
  (let ((s-r (##exact-int.sqrt y)))
    (values (car s-r) (cdr s-r))))

(define-macro (when condition  . body)
  `(if ,condition
       (let ()
	 ,@body)))

(define-macro (unless condition . body)
  `(if (not ,condition)
       (let ()
	 ,@body)))

(define (vector-map f v)
  (let* ((n (vector-length v))
         (result (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n) result)
      (vector-set! result i (f (vector-ref v i))))))

(define write-string write)

(define (this-scheme-implementation-name) (string-append "gambitc-" (system-version-string)))

(define (string->utf8 s)
  (with-output-to-u8vector
   (list char-encoding: 'UTF-8)
   (lambda ()
     (display s))))

(define (utf8->string bstr #!optional (enc 'UTF-8))
  (let* ((in (open-input-u8vector `(char-encoding: ,enc init: ,bstr)))
         (len (u8vector-length bstr))
         (out (make-string len))
         (n (read-substring out 0 len in)))
    (string-shrink! out n)
    out))

(define make-bytevector make-u8vector)

(define bytevector-u8-set! u8vector-set!)
;; TODO: load syntax-case here, to get syntax-rules.
;; google says (load "~~/syntax-case"), but that doesn't work on my machine :-/

;; (define-syntax import
;;   (syntax-rules ()
;;     ((import stuff ...)
;;      (begin) ;; do nothing
;;      )))
