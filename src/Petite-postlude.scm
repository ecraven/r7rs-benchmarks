(define (this-scheme-implementation-name)
  (let* ((machine-type-name (symbol->string (machine-type)))
         (threads (if (char=? (string-ref machine-type-name 0) #\t)
                     "m"
                     "s"))
         (bits (if (char=? (string-ref machine-type-name (if (string=? threads "m") 2 1)) #\6)
                   "64"
                   "32")))
    (string-append "petite-" (call-with-values scheme-version-number
                             (lambda (a b c)
                               (string-append (number->string a)
                                              "."
                                              (number->string b)
                                              "."
                                              (number->string c))))
                            "-" threads bits)))
(define (current-second)
  (time-second (current-time)))
(define (current-jiffy)
  (+ (current-second) (/ (time-nanosecond (current-time)) 1000000000)))
(define (jiffies-per-second) 1)
