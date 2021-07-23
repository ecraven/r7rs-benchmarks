;;; ACK -- One of the Kernighan and Van Wyk benchmarks.

(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "ack"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda () (ack (hide count input1) (hide count input2)))
     (lambda (result) (= result output)))))
