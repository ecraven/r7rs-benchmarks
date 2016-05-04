;;; Extraction of benchmark results.
;;;
;;; The R7RS (scheme time) library has greatly simplified this.
;;; The only outputs we need to recognize look like this:
;;;
;;;     Elapsed time: 3.019585 seconds (3.0) for sum:10000:200000
;;;
;;; where the first time was computed using (current-jiffy),
;;; the second time was computed using (current-second) and
;;; rounded to milliseconds, and the name of the benchmark
;;; follows the " for ".  The benchmark name will always be
;;; a legal symbol in Scheme, so the components of that line
;;; can be read using the read procedure.


(define (summarize-usual-suspects)
  ((summarize r7rs-results) "results.Chibi"   "summary.Chibi")
  ((summarize r7rs-results) "results.Chicken" "summary.Chicken")
  ((summarize r7rs-results) "results.Foment"  "summary.Foment")
  ((summarize r7rs-results) "results.Gauche"  "summary.Gauche")
  ((summarize r7rs-results) "results.Kawa"    "summary.Kawa")
  ((summarize r7rs-results) "results.Larceny" "summary.Larceny")
  ((summarize r7rs-results) "results.Petit"   "summary.Petit"))

(define (decode-usual-suspects)
  (map decode-summary
       '("summary.Larceny"
         "summary.Petit"
         "summary.Chibi"
         "summary.Chicken"
         "summary.Foment"
         "summary.Gauche"
         "summary.Kawa")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (readline port)
  (do ((c (read-char port) (read-char port))
       (chars '() (cons c chars)))
      ((or (eof-object? c)
           (char=? c #\newline))
       (list->string (reverse chars)))))

(define (readlines port)
  (do ((c (peek-char port) (peek-char port))
       (lines '() (cons (readline port) lines)))
      ((eof-object? c)
       (reverse lines))))

; If s1 is a substring of s2, then returns the least integer m
; such that (string=? s1 (substring s2 m (+ m (string-length s1)))).
; Otherwise returns #f.

(define (substring? s1 s2)
  (let ((n1 (string-length s1))
        (n2 (string-length s2)))
    (let ((n (- n2 n1)))
      (let loop ((m 0))
        (if (<= m n)
            (if (substring=? s1 s2 m (+ m n1))
                m
                (loop (+ m 1)))
            #f)))))

(define (substring=? s1 s2 m n)
  (and (<= (string-length s1) (- n m))
       (<= n (string-length s2))
       (do ((i 0 (+ i 1))
            (m m (+ m 1)))
           ((or (= m n)
                (not (char=? (string-ref s1 i)
                             (string-ref s2 m))))
            (= m n)))))

(define (right-justify x n . port)
  (let ((p (open-output-string))
        (port (if (null? port) (current-output-port) (car port))))
    (display x p)
    (let* ((s (get-output-string p))
           (m (string-length s)))
      (if (< m n)
          (display (string-append (make-string (- n m) #\space) s) port)
          (display (substring s 0 n) port)))))

(define (left-justify x n . port)
  (let ((p (open-output-string))
        (port (if (null? port) (current-output-port) (car port))))
    (display x p)
    (let* ((s (get-output-string p))
           (m (string-length s)))
      (if (< m n)
          (display (string-append s (make-string (- n m) #\space)) port)
          (display (substring s 0 n) port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Summarizing the results.* files that are created by the bench
; script.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (summarize f)
  (define (summarize in . rest)
    (define (bad-arguments)
      (error "Bad arguments to summarize-results"
             (cons in rest)))
    (cond ((string? in)
           (call-with-input-file
            in
            (lambda (in) (apply summarize in rest))))
          ((input-port? in)
           (cond ((null? rest)
                  (summarize in (current-output-port)))
                 ((string? (car rest))
                  (call-with-output-file
                   (car rest)
                   (lambda (out)
                     (summarize in out))))
                 ((output-port? (car rest))
                  (f (readlines in) (car rest)))
                 (else
                  (bad-arguments))))
          (else
           (bad-arguments))))
  summarize)

;;; For results displayed in the standard form:
;;;
;;;     Elapsed time: 3.019585 seconds (3.0) for sum:10000:200000

(define (r7rs-results lines out)

  (define (round3 secs)
    (let* ((msec (exact (round (* 1000.0 secs))))
           (secs (quotient msec 1000))
           (msec (remainder msec 1000)))
      (string-append (number->string secs)
                     "."
                     (substring (number->string (+ 1000 msec)) 1 4))))

  (let ((system-key "Benchmarking ")
        (test-key "Elapsed time: "))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (name-width 40)
          (timing-width 10))
      (let loop ((lines lines))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     ;; so we won't do this more than once
                     (set! system-key
                           (make-string n-system-key #\!))
                     (display line out)
                     (newline out)
                     (newline out)
                     (display
                      "benchmark                                     real"
                      out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let* ((p (open-input-string line))
                            (Elapsed (read p))
                            (time: (read p))
                            (secs (read p))
                            (seconds (read p))
                            (rounded (read p))
                            (for (read p))
                            (name (read p)))
                       (assert (and (number? secs)
                                    (list? rounded)
                                    (= 1 (length rounded))
                                    (number? (car rounded))
                                    (symbol? name)))
                       (left-justify name name-width out)
                       (right-justify (round3 secs) timing-width out))))
              (loop (cdr lines))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Conversion of the summaries into Scheme-readable data.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a file name or input port containing a summary
; produced by the summarize procedure above,
; returns a decoded summary of the form
;
; (<system>                   ; a string, e.g. "Larceny"
;  (<hostname> <date> ...)    ; strings
;  ((<benchmark>              ; a symbol, e.g. fib
;    <realtime>)              ; a number, in seconds
;   ...))

(define make-summary list)
(define make-timing list)

(define summary:system car)
(define summary:hostetc cadr)
(define summary:timings caddr)
(define timing:benchmark car)
(define timing:real cadr)

(define (decode-summary in)
  (define (bad-arguments)
    (error "Bad arguments to summarize-results" in))
  (cond ((string? in)
         (call-with-input-file
          in
          (lambda (in) (decode-summary in))))
        ((input-port? in)
         (let skip ((lines (readlines in))
                    (decoded-summaries '()))
           (cond
            ((null? lines) (apply values (reverse decoded-summaries)))
            ((decode-lines lines) => 
             (lambda (decoded+remainder)
               (let* ((rev (reverse decoded+remainder))
                      (remainder (car rev))
                      (decoded (reverse (cdr rev))))
                 (skip remainder (cons decoded decoded-summaries)))))
            (else
             (skip (cdr lines) decoded-summaries)))))
        (else
         (bad-arguments))))

; Given the summary as a list of lines,
; returns the decoded summary as for decode-summary,
; with remaining lines snoc'd on the end

(define (decode-lines lines)
  (let ((system-key "Benchmarking ")
        (date-key " on ")
        (header-key "benchmark"))
    (let ((n-system-key (string-length system-key))
          (n-date-key (string-length date-key))
          (n-header-key (string-length header-key)))
      (define header-line? 
        (lambda (line) (substring=? system-key line 0 n-system-key)))
      (and (not (null? lines))
           (header-line? (car lines))
           (let* ((line0 (car lines))
                  (n0 (string-length line0))
                  (n1 (substring? date-key line0))
                  (system (substring line0 n-system-key n1))
                  (hostname "unknown")
                  (date (substring line0 (+ n1 n-date-key) n0))
                  (benchmark+remaining-lines
                   (let loop ((lines (cdr lines))
                              (bmark-lines '()))
                     (cond ((or (null? lines)
                                (header-line? (car lines)))
                            (list (reverse bmark-lines) lines))
                           (else
                            (loop (cdr lines) 
                                  (cons (car lines) bmark-lines))))))
                  (benchmark-lines (car benchmark+remaining-lines))
                  (remaining-lines (cadr benchmark+remaining-lines))
                  (benchmarks
                   (map (lambda (line)
                          (let* ((padding " #f #f #f #f")
                                 (in (open-input-string
                                      (string-append line padding)))
                                 (name (read in)))
                            (call-with-current-continuation 
                             (lambda (ret)
                               (let loop ((tot-real 0)
                                          (count    0))
                                 (let ((real (read in)))
                                   (cond ((not (number? real))
                                          (cond ((= count 0)
                                                 (list name real))
                                                (else
                                                 (list name
                                                       (/ tot-real count)))))
                                         (else
                                          (loop (+ tot-real real)
                                                (+ count    1))))))))))
                        benchmark-lines))
                  (benchmarks
                   (filter (lambda (x)
                             (and (car x)
                                  (symbol? (car x))
                                  (not (eq? (car x) 'benchmark))
                                  (number? (cadr x))
                                  (positive? (cadr x))))
                           benchmarks)))
             (list system
                   (list hostname date)
                   benchmarks
                   remaining-lines))))))
