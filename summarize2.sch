; Graphical display of benchmark results.
;
; FIXME:  This is awful code.
;
; Typical usage:
;
; % cd Results
; % larceny
; (load "../summarize.sch")
; (load "../summarize2.sch")
;
; (summarize-usual-suspects)
; (define summaries (decode-usual-suspects))
; (graph-benchmarks (summaries-with-geometric-means2 summaries) "temp.linux")

; Given a list of summaries in the representation
; produced by decode-summary, and a filename or
; output port, writes ASCII bar graphs to the file
; or port.

(define (graph-benchmarks summaries out)
  (define (bad-arguments)
    (error "Bad arguments to graph-benchmarks" in))
  (cond ((string? out)
         (call-with-output-file
          out
          (lambda (out) (graph-benchmarks summaries out))))
        ((output-port? out)
         (graph-benchmarks-to-port summaries out))
        (else
         (bad-arguments))))

(define (graph-benchmarks-to-port summaries out)
  (let* ((results (map summary:timings summaries))
         (benchmark-names (map timing:benchmark (car results))))
    (display anchor0 out)
    (for-each (lambda (name)
                (graph-benchmark-to-port name summaries out))
              benchmark-names)
    (display anchor4 out)))

(define width:system 8)
(define width:timing 9)
(define width:gap 2)
(define width:bar 60)
(define width:total (+ width:system width:timing width:gap width:bar))

(define anchor0
   "\n<table class=\"benchmarks\">\n")
(define anchor1
  "\n<tr>\n<td class=\"benchmark\" colspan=\"3\"><a href=\"LINK.html#")
(define anchor2 "\">")
(define anchor3 "</a></td>\n</tr>")
(define anchor4
  "\n</table>\n")

(define (graph-benchmark-to-port name summaries out)

  ; Strips -r6rs-fixflo and similar suffixes from system names.

  (define (short-name system)
    (let* ((rchars (reverse (string->list system)))
           (probe (memv #\- rchars)))
      (if probe
          (short-name (list->string (reverse (cdr probe))))
          system)))    

  (display anchor1 out)
  (display name out)
  (display anchor2 out)
  (display name out)
  (display anchor3 out)
  (newline out)

  (let* ((systems (map summary:system summaries))
         (systems (map short-name systems))
         (results (map summary:timings summaries))
         (timings (map (lambda (x) (assq name x))
                       results))
         (best (apply min
                      1000000000
                      (filter positive?
                              (map timing:real
                                   (filter (lambda (x) x) timings))))))
    (for-each (lambda (system timing)
                (if (list? timing)
                    (graph-system system (timing:real timing) best out)
                    (graph-system system 0 best out)))
              systems
              timings)))

(define graph-system:args '())
(define graph-system:bar1 "<span style=\"background-color:#")
(define graph-system:bar2 "\">")
(define graph-system:bar3 "</span>")

(define graph-system:classes
  '(("Larceny"       "larceny")
    ("Petit"         "petit")
    ("Bigloo"        "bigloo")
    ("Chez"          "chez")
    ("Chibi"         "chibi")
    ("Chicken"       "chicken")
    ("Foment"        "foment")
    ("Gambit"        "gambit")
    ("Gauche"        "gauche")
    ("Ikarus"        "ikarus")
    ("Kawa"          "kawa")
    ("MIT"           "mit")
    ("Mosh"          "mosh")
    ("MzScheme"      "mzscheme")
    ("PLT"           "plt")
    ("Petite"        "petite")
    ("Racket"        "plt")
    ("Scheme48"      "scheme48")
    ("Vicare"        "vicare")
    ("Ypsilon"       "ypsilon")))

(define (system-class system)
  (let ((probe (assoc system graph-system:classes)))
    (if probe (cadr probe) (symbol->string system))))

(define graph-system:names
  '(("Larceny"       "Larceny")
    ("Petit"         "Petit Larceny")
    ("Bigloo"        "Bigloo")
    ("Chez"          "Chez")
    ("Chibi"         "Chibi")
    ("Chicken"       "Chicken")
    ("Foment"        "Foment")
    ("Gambit"        "Gambit")
    ("Gauche"        "Gauche")
    ("Ikarus"        "Ikarus")
    ("Kawa"          "Kawa")
    ("MIT"           "MIT Scheme")
    ("Mosh"          "Mosh")
    ("MzScheme"      "MzScheme")
    ("PLT"           "PLT")
    ("Petite"        "Petite Chez")
    ("Racket"        "Racket")
    ("Scheme48"      "Scheme 48")
    ("Vicare"        "Vicare")
    ("Ypsilon"       "Ypsilon")))

(define (system-name system)
  (let ((probe (assoc system graph-system:names)))
    (if probe (cadr probe) (symbol->string system))))

(define (graph-system system timing best out)
  (if (and (number? timing)
           (positive? timing))
      (let ((relative (/ best timing))
            (timing (/ (round (* 1000.0 timing)) 1000.0)))

        (display "<tr>\n<td>" out)
        (display (system-name system) out)
        (display "</td>\n" out)

        (display "<td>" out)
        (right-justify timing width:timing out)
        (display "</td>\n" out)

        (display "<td class=\"bar\"><div class=\"" out)
        (display (system-class system) out)
        (display "\" style=\"width: " out)
        (display (inexact->exact (round (* 100 relative))) out)
        (display "%\">&nbsp;</td>\n</tr>\n" out))

      (begin
        (display "<tr>\n<td>" out)
        (display system out)
        (display "</td>\n</tr>\n" out))))

; Given a timing in milliseconds,
; returns the timing in seconds,
; as a string rounded to two decimal places.

(define (msec->seconds t)
  (let* ((hundredths (inexact->exact (round (/ t 10.0))))
         (s (number->string hundredths))
         (n (string-length s)))
    (cond ((>= n 2)
           (string-append (substring s 0 (- n 2))
                          "."
                          (substring s (- n 2) n)))
          (else
           (string-append ".0" s)))))

; Given a summary and a list of summaries,
; returns a list of relative performance (0.0 to 1.0)
; for every benchmark in the summary.

(define (relative-performance summary summaries)

  (let* ((timings (summary:timings summary))
         (timings (filter (lambda (t)
                            (let ((realtime (timing:real t)))
                              (and (number? realtime)
                                   (positive? realtime))))
                          timings))
         (other-results (map summary:timings summaries)))
    (map (lambda (t)
           (let* ((name (timing:benchmark t))
                  (timings (map (lambda (x) (assq name x))
                                other-results))
                  (best (apply min
                               (map timing:real
                                    (filter (lambda (x) x) timings)))))
             (/ best (timing:real t))))
         timings)))

; Same as above, but assigns an arbitrary relative performance
; when the timing is absent.

(define *arbitrary-relative-performance* 0.1)

(define (relative-performance2 summary summaries names)

  (let* ((timings (summary:timings summary))
         (timings (map (lambda (t)
                         (let ((realtime (timing:real t)))
                           (if (and (number? realtime)
                                    (positive? realtime))
                               t
                               (make-timing (timing:benchmark t) 0 0 0))))
                       timings))
         (timings (map (lambda (name)
                         (let ((t (assq name timings)))
                           (if t t (make-timing name 0 0 0))))
                       names))
         (other-results (map summary:timings summaries)))
    (map (lambda (name t)
           (let* ((timings (map (lambda (x) (assq name x))
                                other-results))
                  (timings (filter positive?
                                   (map timing:real
                                        (filter (lambda (x) x)
                                                timings))))
                  (best (if (null? timings) 1 (apply min timings)))
                  (worst (if (null? timings) 1 (apply max timings))))
             (let ((realtime (timing:real t)))
               (if (positive? realtime)
                   (/ best realtime)
                   (begin (display "No positive timing: ")
                          (write name)
                          (newline)
                          (* *arbitrary-relative-performance*
                             (/ best worst)))))))
         names timings)))

; Given a list of positive numbers,
; returns its geometric mean.

(define (geometric-mean xs)
  (if (null? xs)
      1
      (expt (apply * xs) (/ 1 (length xs)))))

; Given a list of summaries, returns a list of summaries
; augmented by the geometric mean over all benchmarks.

(define (summaries-with-geometric-means summaries)

  (define name (string->symbol "geometricMean"))

  (map (lambda (summary)
         (define mean
           (/ (geometric-mean (relative-performance summary summaries))))
         (make-summary (summary:system summary)
                       (summary:hostetc summary)
                       (cons
                        (make-timing name mean mean 0)
                        (summary:timings summary))))
       summaries))

; Same as above, but uses relative-performance2.
;
; FIXME: assumes the first implementation in the list
; has a timing for every benchmark.

(define (summaries-with-geometric-means2 summaries)

  (define name (string->symbol "geometricMean"))

  (map (lambda (summary)
         (define mean
           (/ (geometric-mean
               (relative-performance2 summary
                                      summaries
                                      (map timing:benchmark
                                           (summary:timings
                                            (car summaries)))))))
         (make-summary (summary:system summary)
                       (summary:hostetc summary)
                       (cons
                        (make-timing name mean mean 0)
                        (summary:timings summary))))
       summaries))
