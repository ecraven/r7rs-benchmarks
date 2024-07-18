(import 
  (except (ironscheme) time define-record-type partition)
  (only (rename (ironscheme) (define-record-type r6rs:define-record-type)))
  (srfi :19) 
  (srfi :112))

(define-syntax define-record-type
  ;; Simplistic R7RS to R6RS define-record-type transformation.
  (syntax-rules ()
    ((define-record-type name
      (make-name param ...)
      name?
      (field-name field-ref field-set!)
      ...)
     (r6rs:define-record-type (name make-name name?)
       (protocol
         (lambda (new)
           (lambda (param ...)
             (new param ...))))
       (fields
         (mutable field-name field-ref field-set!)
         ...)))))

(define (exact-integer? i)
  (and (integer? i) (exact? i)))
(define (square x)
  (* x x))
(define (read-line port)
  (get-line port))
(define (jiffies-per-second)
  1)
(define (current-jiffy)
  (let ((t (current-time)))
    (+ (time-second t) (/ (time-nanosecond t) 1000000000))))
(define (current-second)
  (time-second (current-time)))
(define (write-string str port)
  (put-string port str))
(define (this-scheme-implementation-name)
  (string-append "ironscheme-" (ironscheme-version)))
