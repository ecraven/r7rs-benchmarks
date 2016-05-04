(define (this-scheme-implementation-name)
  (string-append "petite-" (call-with-values scheme-version-number
                           (lambda (a b c)
                             (string-append (number->string a)
                                            "."
                                            (number->string b)
                                            "."
                                            (number->string c))))))
(define (current-second)
  (time-second (current-time)))
(define (current-jiffy)
  (+ (current-second) (/ (time-nanosecond (current-time)) 1000000000)))
(define (jiffies-per-second) 1)
