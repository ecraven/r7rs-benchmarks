(define (this-scheme-implementation-name)
  (let* ((machine-type-name (symbol->string (machine-type)))
         (threads (if (char=? (string-ref machine-type-name 0) #\t)
                     "multithreaded"
                     "singlethreaded"))
         (bits (if (char=? (string-ref machine-type-name (if (string=? threads "multithreaded") 2 1)) #\6)
                   "64bit"
                   "32bit")))
    (string-append "chez-" (call-with-values scheme-version-number
                             (lambda (a b c)
                               (string-append (number->string a)
                                              "."
                                              (number->string b)
                                              "."
                                              (number->string c))))
                            "-" bits
                            "-" threads)))
(define (current-second)
  (time-second (current-time)))
(define (current-jiffy)
  (+ (current-second) (/ (time-nanosecond (current-time)) 1000000000)))
(define (jiffies-per-second) 1)
