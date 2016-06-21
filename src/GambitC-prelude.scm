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
;; TODO: load syntax-case here, to get syntax-rules.
;; google says (load "~~/syntax-case"), but that doesn't work on my machine :-/

;; (define-syntax import
;;   (syntax-rules ()
;;     ((import stuff ...)
;;      (begin) ;; do nothing
;;      )))
