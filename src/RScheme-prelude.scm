(define (jiffies-per-second) 1)
(define (current-second) (with-module syscalls (call-with-values (lambda () (time->calendar (time) #f))
                                                 (lambda (year month month-day hour min sec year-day week-day dst?)
                                                   (+ sec (* min 60) (* hour 60 60))))))
(define (current-jiffy) (+ (current-second) (with-module syscalls (/ (time-microseconds (time)) 1000000))))
(define inexact exact->inexact)
(define exact inexact->exact)
(define (this-scheme-implementation-name)
  (string-append "rscheme-" "unknown"))
(define-syntax when
  (syntax-form (test body0 . body)
               (if test
                   (begin body0 . body)
                   #f)))
(define-syntax unless
  (syntax-form (test body0 . body)
               (if (not test)
                   (begin body0 . body)
                   #f)))
(define (square x) (* x x))
(define (exact-integer? x)
  (and (exact? x)
       (integer? x)))
;; file-exists? -> posix / file-accesses
;; delete-file -> posix / unlink

