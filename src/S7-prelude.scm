;;;; kindly provided by mgubi (https://github.com/ecraven/r7rs-benchmarks/issues/55)
(define (this-scheme-implementation-name) "s7")
(define exact-integer? integer?)
(define (exact-integer-sqrt i) (let ((sq (floor (sqrt i)))) (values sq (- i (* sq sq)))))
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square x) (* x x))
(define (vector-map f v) (copy v)) ; for quicksort.scm
(define-macro (import . args) #f)
(define (jiffies-per-second) 1000)
(define (current-jiffy) (round (* (jiffies-per-second) (*s7* 'cpu-time))))
(define (current-second) (floor (*s7* 'cpu-time)))

(define read-u8 read-byte)
(define write-u8 write-byte)
(define u8-ready? char-ready?)
(define peek-u8 peek-char)
(define* (utf8->string v (start 0) end)
  (if (string? v)
      v
      (substring (byte-vector->string v) start (or end (length v)))))
(define* (string->utf8 s (start 0) end)
  (if (byte-vector? s)
      s
      (string->byte-vector (utf8->string s start end))))
(define write-simple write)


(define* (string->vector s (start 0) end)
  (let ((stop (or end (length s))))
    (copy s (make-vector (- stop start)) start stop)))

(define vector-copy string->vector)
(define* (vector-copy! dest at src (start 0) end) ; end is exclusive
  (let ((len (or end (length src))))
    (if (or (not (eq? dest src))
            (<= at start))
        (do ((i at (+ i 1))
             (k start (+ k 1)))
            ((= k len) dest)
          (set! (dest i) (src k)))
        (do ((i (- (+ at len) start 1) (- i 1))
             (k (- len 1) (- k 1)))
            ((< k start) dest)
          (set! (dest i) (src k))))))

(define make-bytevector make-byte-vector)
(define bytevector-ref byte-vector-ref)
(define bytevector-set! byte-vector-set!)
(define bytevector-copy! vector-copy!)
(define bytevector-u8-ref byte-vector-ref)
(define bytevector-u8-set! byte-vector-set!)

;; records
(define-macro (define-record-type type make ? . fields)
  (let ((obj (gensym))
        (args (map (lambda (field)
                     (values (list 'quote (car field))
                             (let ((par (memq (car field) (cdr make))))
                               (if (pair? par) (car par) #f))))
                   fields)))
    `(begin
       (define (,? ,obj)
         (and (let? ,obj)
              (eq? (let-ref ,obj 'type) ',type)))
       
       (define ,make 
         (inlet 'type ',type ,@args))

       ,@(map
          (lambda (field)
            (when (pair? field)
              (if (null? (cdr field))
                  (values)
                  (if (null? (cddr field))
                      `(define (,(cadr field) ,obj)
                         (let-ref ,obj ',(car field)))
                      `(begin
                         (define (,(cadr field) ,obj)
                           (let-ref ,obj ',(car field)))
                         (define (,(caddr field) ,obj val)
                           (let-set! ,obj ',(car field) val)))))))
          fields)
       ',type)))
