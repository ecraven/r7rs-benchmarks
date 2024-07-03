(import (scheme base)
        (srfi :69)
        (only (mit-compat) read-file-lines burst-string first second third symbol<? delete-duplicates round->exact truncate->exact write-to-string floor->exact)
        (only (srfi :1) delete any count iota filter-map append-map list-index)
        (only (srfi :13) string-prefix?)
        (rename (only (chezscheme) sort current-time time-utc->date) (sort %sort))
        (sxml-to-xml))
(define (sort list predicate)
  (%sort predicate list))
(define html-nl "&#10;")
(define *descriptions* (make-hash-table))
(define-record-type :description
  (make-description name type description tags)
  description?
  (name description-name)
  (type description-type)
  (description description-description)
  (tags description-tags))
(define (define-description name type description . tags)
  (hash-table-set! *descriptions* name (make-description name type description tags)))
;;(define-description "total-accumulated-runtime" "Stats" "The total accumulated runtime of all tests that all Schemes finished.")
(define-description "times-faster-than-any-other" "Stats" "The number of times an implementation was the fastest of all on a test.")
(define-description "tests-finished" "Stats" "The number of tests an implementation has finished successfully.")
(define-description "browse" "Gabriel" "Browsing a data base, a Gabriel benchmark, 1000 iterations. [May be a test of string->symbol and/or symbol->string.]")
(define-description "deriv" "Gabriel" "Symbolic differentiation, a Gabriel benchmark, ten million iterations.")
(define-description "dderiv" "Gabriel" "Table-driven symbolic differentiation, a Gabriel benchmark, ten million iterations. Uses hashtables and association lists instead of the original benchmark's property lists.")
(define-description "destruc" "Gabriel" "Destructive list operations, a Gabriel benchmark, 1000 iterations of a 600x50 problem." 'destructive-list-operations)
(define-description "diviter" "Gabriel" "Divides 1000 by 2 using lists as a unary notation for integers, a Gabriel benchmark, one million iterations. This benchmark tests null?, cons, car, cdr, and little else." 'list-operations)
(define-description "divrec" "Gabriel" "This benchmark is the same as diviter except it uses deep recursion instead of iteration.")
(define-description "puzzle" "Gabriel" "Combinatorial search of a state space, a Gabriel benchmark, 500 iterations. A test of arrays and classical compiler optimizations. This benchmark was originally written in Pascal by Forrest Baskett.")
(define-description "triangl" "Gabriel" "Another combinatorial search similar to puzzle, a Gabriel benchmark, 50 iterations.")
(define-description "tak" "Gabriel" "A triply recursive integer function related to the Takeuchi function, a Gabriel benchmark. 10 iterations of (tak 32 16 8). A test of non-tail calls and arithmetic. [Historical note: The Symbolics 3600 performed 1 iteration of (tak 18 12 6) in 0.43 seconds using generic arithmetic. On our test machine, Larceny runs that benchmark in 0.00016 seconds. That's 2500 times as fast.]")
(define-description "takl" "Gabriel" "The tak:32:16:8 benchmark using lists to represent integers, a Gabriel benchmark (with different arguments), 2 iterations.")
(define-description "ntakl" "Gabriel" "The takl benchmark contains a peculiar boolean expression. Rewriting that expression into a more readable idiom allows some compilers to generate better code for it.")
(define-description "cpstak" "Gabriel" "The tak:32:16:8 benchmark in continuation-passing style, 5 iterations. A test of closure creation.")
(define-description "ctak" "Gabriel" "The tak:32:16:8 benchmark in continuation-capturing style, 1 iteration. A test of call-with-current-continuation. [Larceny's code for call-with-current-continuation is now written in C, and most of its time on this benchmark is spent crossing the Scheme/C barrier.]")

(define-description "fib" "Numeric" "Doubly recursive computation of the 40th fibonacci number (102334155), using (< n 2) to terminate the recursion; 1 iteration.")
(define-description "fibc" "Numeric" "A version of fib that uses first class continuations; written by Kent Dybvig. Calculates the 30th Fibonacci number (832040) 10 times.")
(define-description "fibfp" "Numeric" "Calculation of the 35th Fibonacci number using inexact numbers; 10 iterations. A test of floating point arithmetic. Uses essentially the same code as the fib benchmark.")
(define-description "sum" "Numeric" "Sums the integers from 0 to 10000, 100000 iterations.")
(define-description "sumfp" "Numeric" "Sums the integers from 0 to 1e6, 250 iterations. A test of floating point arithmetic. Uses essentially the same code as the sum benchmark.")
(define-description "fft" "Numeric" "Fast Fourier Transform on 65536 real-valued points, 50 iterations. A test of floating point arithmetic.")
(define-description "mbrot" "Numeric" "Generation of a Mandelbrot set, 1000 iterations on a problem of size 75. A test of floating point arithmetic on reals.")
(define-description "mbrotZ" "Numeric" "Same as the mbrot benchmark, but using complex instead of real arithmetic.")
(define-description "mbrot" "Numeric" "Generation of a Mandelbrot set, 1000 iterations on a problem of size 75. A test of floating point arithmetic.")
(define-description "nucleic" "Numeric" "Determination of a nucleic acid's spatial structure, 50 iterations. A test of floating point arithmetic, and a real program.")
(define-description "pnpoly" "Numeric" "Testing to see whether a point is contained within a 2-dimensional polygon, 500000 iterations (with 12 tests per iteration). A test of floating point arithmetic.")
(define-description "ack" "Kernighan-Van-Wyk" "A version of the Ackermann function, with arguments 3,12. One iteration.")
(define-description "array1" "Kernighan-Van-Wyk" "This benchmark allocates, initializes, and copies some fairly large one-dimensional arrays. 100 iterations on a problem size of one million.")
(define-description "string" "Kernighan-Van-Wyk" "This tests string-append and substring, and very little else. 10 iterations on a problem size of 500000.")
(define-description "sum1" "Kernighan-Van-Wyk" "This benchmark reads and sums 100,000 floating point numbers ten times. It is primarily a test of floating point input.")
(define-description "cat" "Kernighan-Van-Wyk" "This file-copying benchmark is a simple test of character I/O. It copies the King James Bible 25 times.")
(define-description "cat2" "Kernighan-Van-Wyk" "Same as cat except it uses UTF-8 transcoding instead of Latin-1.")
(define-description "cat3" "Kernighan-Van-Wyk" "Same as cat except it uses UTF-16 transcoding instead of Latin-1.")
(define-description "tail" "Kernighan-Van-Wyk" "This benchmark performs considerable character i/o. It prints the King James Bible verse by verse, in reverse order of the verses, ten times.")
(define-description "wc" "Kernighan-Van-Wyk" "Another character i/o benchmark. It counts the number of words in the King James Bible 25 times.")
(define-description "read0" "Input/Output" "This synthetic benchmark tests the read procedure on all 1-character inputs and on all 2-character inputs that begin with #\a. Since most such inputs are illegal, this is largely a test of R6RS exception handling.")
(define-description "read1" "Input/Output" "Reads nboyer.sch 2500 times using Latin-1 transcoding.")
(define-description "read2" "Input/Output" "Reads nboyer.sch 2500 times using UTF-8 transcoding.")
(define-description "read3" "Input/Output" "Reads nboyer.sch 2500 times using UTF-16 transcoding.")
(define-description "bibfreq" "Others" "Uses eq? hashtables to find the words that occur most frequently in the King James Bible.")
(define-description "bibfreq2" "Others" "Uses symbol-hash hashtables to find the words that occur most frequently in the King James Bible.")
(define-description "compiler" "Others" "A compiler kernel that looks as though it was written by Marc Feeley. 1000 iterations on a 47-line input. [Although Larceny/IA32 is able to run this benchmark, Larceny/SPARC cannot compile it due to its assumption that stack frames are smaller than 4096 bytes.]")
(define-description "conform" "Others" "A type checker written by Jim Miller, 200 iterations.")
(define-description "dynamic" "Others" "Dynamic type inference, self-applied, 200 iterations. Written by Fritz Henglein. A real program.")
(define-description "earley" "Others" "Earley's parsing algorithm, parsing a 15-symbol input according to one of the simplest ambiguous grammars, 1 iteration. A real program, applied to toy data whose exponential behavior leads to a peak heap size of half a gigabyte or more.")
(define-description "graphs" "Others" "This program was provided by Andrew Wright, but we don't know much about it, and would appreciate more information. This higher order program creates closures almost as often as it performs non-tail procedure calls. One iteration on a problem of size 7.")
(define-description "lattice" "Others" "Another program that was provided by Andrew Wright, though it may have been written by Jim Miller. It enumerates the order-preserving maps between finite lattices. 10 iterations.")
(define-description "matrix" "Others" "Another program that was provided by Andrew Wright. Computes maximal matrices; similar to some puzzle programs. 1000 iterations on a problem of size 5.")
(define-description "maze" "Others" "Constructs a maze on a hexagonal grid, 5000 iterations. Written by Olin Shivers.")
(define-description "mazefun" "Others" "Constructs a maze on a rectangular grid using purely functional style, 5000 iterations on a problem of size 11. Written by Marc Feeley.")
(define-description "nqueens" "Others" "Computes the number of solutions to the 13-queens problem, 10 times.")
(define-description "paraffins" "Others" "Computes the number of paraffins that have 23 carbon atoms, 5 times.")
(define-description "parsing" "Others" "Parses the nboyer benchmark 1000 times using a scanner and parser generated using Will Clinger's LexGen and ParseGen.")
(define-description "peval" "Others" "Partial evaluation of Scheme code, 1000 iterations. Written by Marc Feeley.")
(define-description "pi" "Others" "A bignum-intensive benchmark that calculates digits of pi.")
(define-description "primes" "Others" "Computes the primes less than 1000, 5000 times, using a list-based Sieve of Eratosthenes. Written by Eric Mohr.")
(define-description "quicksort" "Others" "This is a quicksort benchmark. (That isn't as obvious as it sounds. The quicksort benchmark distributed with Gambit is a bignum benchmark, not a quicksort benchmark. See the comments in the code.) Sorts a vector of 10000 random integers 2500 times. Written by Lars Hansen, and restored to its original glory by Will Clinger.")
(define-description "ray" "Others" "Ray tracing a simple scene, 20 iterations. A test of floating point arithmetic. This program is translated from the Common Lisp code in Example 9.8 of Paul Graham's book on ANSI Common Lisp.")
(define-description "scheme" "Others" "A Scheme interpreter evaluating a merge sort of 30 strings, 100000 iterations. Written by Marc Feeley.")
(define-description "simplex" "Others" "Simplex algorithm, one million iterations. A test of floating point arithmetic, and a real program.")
(define-description "slatex" "Others" "Scheme to LaTeX processor, 100 iterations. A test of file i/o and probably much else. Part of a real program written by Dorai Sitaram.")
(define-description "nboyer" "Garbage Collection" "An updated and exponentially scalable version of the boyer benchmark. The nboyer benchmark's data structures are considerably more appropriate than the data structures used in the boyer benchmarks. These timings are for 1 iteration on a problem of size 4. A test of lists, vectors, and garbage collection.")
(define-description "sboyer" "Garbage Collection" "A version of nboyer that has been tuned (by Henry Baker) to reduce storage allocation, making it less of a garbage collection benchmark and more of a compiler benchmark. Only 4 lines of code were changed, and another 7 lines of code were added. These timings are for 1 iteration on a problem of size 5.")
(define-description "gcbench" "Garbage Collection" "This program was written to mimic the phase structure that has been conjectured for a class of application programs for which garbage collection may represent a significant fraction of the execution time. This benchmark warms up by allocating and then dropping a large binary tree. Then it allocates a large permanent tree and a permanent array of floating point numbers. Then it allocates considerable tree storage in seven phases, increasing the tree size in each phase but keeping the total storage allocation approximately the same for each phase. Each phase is divided into two subphases. The first subphase allocates trees top-down using side effects, while the second subphase allocates trees bottom-up without using side effects. This benchmark was written in Java by John Ellis and Pete Kovac, modified by Hans Boehm, and translated into Scheme, Standard ML, C++, and C by William Clinger. The timings shown are for 1 iteration on problem size 20.")
(define-description "mperm" "Garbage Collection" "The mperm20:9:2:1 benchmark is a severe test of storage allocation and garbage collection. At the end of each of the 20 iterations, the oldest half of the live storage becomes garbage. This benchmark is particularly difficult for generational garbage collectors, since it violates their assumption that young objects have a shorter future life expectancy than older objects. The perm9 benchmark distributed with Gambit does not have that property.")
(define-description "gcold" "Garbage Collection" "A synthetic garbage collection benchmark written by David Detlefs and translated to Scheme by Will Clinger and Lars Hansen.")
(define-description "equal" "R6RS" "This benchmark tests the R6RS equal? predicate on some fairly large structures of various shapes.")
(define-description "normalization" "R6RS" "This benchmark runs all of the Unicode 5.0.0 tests of string normalization.")
(define-description "bv2string" "R6RS" "This benchmark tests conversions between bytevectors and Unicode.")
(define-description "listsort" "R6RS" "This benchmark tests the list-sort procedure. Otherwise it is the same as the vecsort benchmark.")
(define-description "vecsort" "R6RS" "This benchmark tests the vector-sort procedure. Otherwise it is the same as the listsort benchmark.")
(define-description "hashtable0" "R6RS" "This is a synthetic benchmark designed to stress hashtables.")

(define (get-description name)
  (hash-table-ref/default *descriptions* name (make-description name "" "no description" '())))

(define (read-data)
  (map (lambda (line) (let ((parts (burst-string line #\, #f)))
                   (list (string->symbol (car (burst-string (string-downcase (second parts)) #\: #f)))
                         (string->symbol (string-downcase (first parts)))
                         (if (string->number (third parts))
                             (string->number (third parts))
                             (string->symbol (string-downcase (third parts)))))))
       (read-file-lines "all.csv")))

(define data (read-data))
(define schemes (let ((all (delete-duplicates (sort (map second data) symbol<?))))
                  (define (symbol-prefix? a b)
                    (string-prefix? (symbol->string a) (symbol->string b)))
                  (filter (lambda (scheme)
                            (not (any (lambda (other)
                                        (symbol-prefix? scheme other))
                                      (delete scheme all))))
                          all)))
(define tests (delete-duplicates (sort (map first data) symbol<?)))
(define tests-with-results-for-every-scheme
  (filter (lambda (test)
            (let* ((runs (filter (lambda (el) (and (eq? (car el) test) (number? (third el)))) data))
                   (this-test-schemes (map cadr runs)))
              (= (length schemes) (length this-test-schemes))
              ))
          tests))
(define total-time (map (lambda (scheme)
                          (list scheme (inexact (round->exact (apply + (map (lambda (test)
                                                                              (if (and (eq? (second test) scheme)
                                                                                       (memq (first test) tests-with-results-for-every-scheme))
                                                                                  (third test)
                                                                                  0))
                                                                            data))))))
                        schemes))

(define test-time (map (lambda (test)
                         (cons test (map cdr
                                         (filter (lambda (el)
                                                   (and (eq? (car el) test)
                                                        (number? (third el))))
                                                 data))))
                       tests))
(define times-fastest (let ((res (map (lambda (run)
                                        (let ((sorted (sort (cdr run) (lambda (a b) (< (second a) (second b))))))
                                          (if (null? sorted)
                                              #f
                                              (caar sorted))))
                                      test-time)))
                        (map (lambda (scheme)
                               (list scheme (count (lambda (el) (eq? el scheme)) res)))
                             schemes)))
(define (test-ranks test)
  (let ((sorted (sort (cdr test) (lambda (a b) (< (second a) (second b))))))
    (cons (car test)
          (filter-map (lambda (s i)
                        (if (> i 9)
                            #f
                            (cons (car s) i)))
                      sorted
                      (iota (length sorted) 1)))))
(define (number-list< a b)
  "compare '(foo 1 1 2 3) to '(bar 1 2 3), return #t if smaller."
  (let ((tests 57)
        (score 10))
    (define (t l)
      (apply + (* score (- tests (length l)))
             l))
    (< (t (cdr a)) (t (cdr b))))
  ;; (let loop ((a (cdr a)) (b (cdr b)))
  ;;   (cond ((and (null? a)
  ;;               (null? b))
  ;;          #f)
  ;;         ((and (null? a)
  ;;               (not (null? b)))
  ;;          #f)
  ;;         ((and (not (null? a))
  ;;               (null? b))
  ;;          #t)
  ;;         (else
  ;;          (cond ((< (car a) (car b))
  ;;                 #t)
  ;;                ((> (car a) (car b))
  ;;                 #f)
  ;;                (else
  ;;                 (loop (cdr a) (cdr b)))))))
  )
(define (summarized-test-ranks tests)
  (let ((t (make-hash-table)))
    (for-each (lambda (el)
                (hash-table-set! t (car el) (cons (cdr el) (hash-table-ref/default t (car el) '()))))
              (append-map cdr
                          (map test-ranks tests)))
    (sort (map (lambda (el)
                 (cons (car el) (sort (cdr el) <)))
               (hash-table->alist t))
          number-list<)))
(define (aggregate-two-schemes one two new-name)
  (define (safe-cadr x)
    (if (and (list? x)
             (> (length x) 1))
        (cadr x)
        #f))
  (let ((t1 (or (safe-cadr one) 9999999999))
        (t2 (or (safe-cadr two) 9999999999)))
    (list new-name (min t1 t2))))
(define (special-aggregates lst)
  (map (lambda (test)
         (let* ((ger (find (lambda (el)
                             (string-prefix? "gerbil" (symbol->string (car el))))
                           (cdr test)))
                (gamb (find (lambda (el)
                              (string-prefix? "gambit" (symbol->string (car el))))
                            (cdr test))))
           (cons (car test)
                 (cdr test)
                 ;; (cons (aggregate-two-schemes ger gamb 'gambit/gerbil)
                 ;;       (delete gamb
                 ;;               (delete ger
                 ;;                       (cdr test))))
                 )))
       lst))
(define tests-ranks (summarized-test-ranks (special-aggregates test-time)))
(define tests-finished
  (sort (map (lambda (scheme)
               (list scheme (count (lambda (el) (and (eq? (second el) scheme)
                                                     (number? (third el))))
                                   data)))
             schemes)
        (lambda (a b)
          (> (second a) (second b)))))
(set! test-time ;cons ; (cons 'total-accumulated-runtime total-time)
      (append `(;;(times-faster-than-any-other ,@times-fastest)
                ;; (tests-finished ,@tests-finished)
                )
              test-time))
(define (nice-number n)
  (let ((num (write-to-string n)))
    (if (char=? (string-ref num (-1+ (string-length num))) #\.)
        (string num "0")
        num)))

(define rank-colors
  (list "#54af36"
        "#69ce30"
        "#89d83e"
        "#ace854"
        "#c7f265"
        "#ffe35b"
        "#f2c747"
        "#e8ad37"
        "#e58922"))
;; /tmp/Untitled_5714630_lrg.jpg
(define colors (list "#eb7454" "#ef8f3b" "#ffe35b" "#c7f265" "#77ff71" "#52f0cf" "#dc5feb"
                     ;;                     "#e35642" "#e87a2b" "#f2c747" "#afe654" "#43e872" "#3cd7c5" "#c549db"
                     "#d73f34" "#dc5e21" "#e8ad37" "#89d83e" "#2ed772" "#2bc7c6" "#a73bcd"
                     ;;                     "#cc2626" "#cd4012" "#e58922" "#69ce30" "#1ec679" "#1dabb7" "#902dc5"
                     "#952531" "#ab2c19" "#d8732f" "#54af36" "#25a37e" "#2a869f" "#742fa3"
                     ;;                     "#722336" "#8f2320" "#bf5c35" "#3a9641" "#278479" "#2c6f8c" "#5c2c92"
                     "#562334" "#631f20" "#964f3d" "#347557" "#256362" "#305872" "#432c62"))
;; flatcolorsui.com
;; (define colors (list "#1abc9c" "#16a085" "#2ecc71" "#27ae60" "#3498db" "#2980b9" "#34495e" "#2c3e50" "#ea4c88" "#ca2c68" "#9b59b6" "#8e44ad" "#f1c40f" "#f39c12" "#e74c3c" "#c0392b" "#ecf0f1" "#bdc3c7" "#95a5a6" "#7f8c8d"))
;; flatuicolors.com
;; (define colors (list "#1abc9c" "#2ecc71" "#3498db" "#9b59b6" "#34495e"
;;                      "#16a085" "#27ae60" "#2980b9" "#8e44ad" "#2c3e50"
;;                      "#f1c40f" "#e67e22" "#e74c3c" "#ecf0f1" "#95a5a6"
;;                      "#f39c12" "#d35400" "#c0392b" "#bdc3c7" "#7f8c8d"))

(define (scheme-type scheme)
  (case scheme
    ((chez larceny mit vicare)
     "native-compiler")
    ((bigloo chicken gambit rhizome rscheme)
     "compile-to-c")
    (else
     "other")))
(define (format-test test)
  (let* ((width 320)
         (row-width 300)
         (name (car test))
         (runs (cdr test))
         (times (map second runs))
         (max-time (let ((m (apply max (if (null? times) '(1) times))))
                     (if (zero? m) 1 m))))
    (if (null? runs)
        ""
        (let* ((d (get-description (car (burst-string (symbol->string name) #\: #f))))
               (name (description-name d))
               (type (description-type d))
               (description (description-description d))
               (fastest-runtime (second (car (sort runs (lambda (a b) (< (second a) (second b))))))))
          (when (zero? fastest-runtime)
            (error "fastest run is 0" test))
          `(div (@ (class "testrun")) ;; (@ (style ,(string "width: 440px; height: " (* 25 (length schemes)) "px")))
                (h4 (@ (title ,description))
                    ,name
                    " ["
                    (small ,type)
                    "] "
                    (span (@ (title ,description))
                          " (?)"))
                (svg (@ (class "chart")
                        (width ,(number->string width))
                        (height ,(* 22 (length runs))))
                     ,@(map (lambda (run index)
                              (let* ((scheme (first run))
                                     (runtime (/ (round (* (second run) 100)) 100))
                                     (adjusted-time (inexact (* row-width (/ runtime max-time))))
                                     (color (list-ref (append rank-colors (list "#cccccc"))
                                                      (min (length rank-colors)
                                                           (floor->exact (/ runtime (* fastest-runtime 10)))))
                                            ;;(list-ref colors (modulo (list-index (lambda (el) (eq? el scheme)) schemes) (length colors)))
                                            ))
                                `(g (@ (transform ,(string-append "translate(0," (number->string (* 20 index)) ")"))
                                       (class ,(let ((sch (car (burst-string (symbol->string scheme) #\- #f))))
                                                 (string-append sch " " (scheme-type (string->symbol sch))))))
                                    ;; (title "") ;; TODO
                                    (rect (@ (width ,(nice-number adjusted-time))
                                             (height 19)
                                             (style ,(string-append "fill:" color))))
                                    (text (@ (x ,(nice-number (if (> adjusted-time (/ width 2)) 5 (+ adjusted-time 5))))
                                             (y 9.5)
                                             (dy ".35em"))
                                          ,(symbol->string scheme)
                                          " ("
                                          ,runtime
                                          ")"))))
                            (sort runs (lambda (a b) (< (second a) (second b))))
                            (iota (length runs)))))))))

;; <script src=\"https://cdnjs.cloudflare.com/ajax/libs/zepto/1.1.6/zepto.min.js\"></script>

(define (scheme-position-in-test scheme test)
  (let* ((data (sort (filter (lambda (el) (and (eq? (car el) test) (number? (third el)))) data)
                     (lambda (a b)
                       (< (third a) (third b)))))
         (index (list-index (lambda (el)
                              (eq? scheme (second el)))
                            data)))
    (if index
        (cons index (length data))
        (cons -1 (length data)))))

(define (scheme-position-color nr total)
  ;; (let ((r (exact (inexact (truncate (+ 128 (* 127 (/ nr total))))))))
  ;;   (string (number->string r 16) "ff" "77"))
  (list-ref rank-colors (truncate->exact (* (/ nr total) (- (length rank-colors) 1)))))

(define (scheme-percentage-in-test scheme test)
  (let* ((data (sort (filter (lambda (el) (and (eq? (car el) test) (number? (third el)))) data)
                     (lambda (a b)
                       (< (third a) (third b)))))
         (slowest (apply max (map third data)))
         (this-scheme (find (lambda (el) (eq? (second el) scheme)) data))
         (this-time (if this-scheme (third this-scheme) #f)))
    (if this-time
        (/ (round (* 10 (inexact (* 100 (/ this-time slowest))))) 10)
        #f)))
(define (scheme-fast-percentage-in-test scheme test)
  (let* ((data (sort (filter (lambda (el) (and (eq? (car el) test) (number? (third el)))) data)
                     (lambda (a b)
                       (< (third a) (third b)))))
         (fastest (apply min (map third data)))
         (this-scheme (find (lambda (el) (eq? (second el) scheme)) data))
         (this-time (if this-scheme (third this-scheme) #f)))
    (if this-time
        (/ (round (* 10 (inexact (* 100 (/ this-time fastest))))) 10)
        #f)))

(define (nice-float n)
  (let ((num (number->string n)))
    (if (char=? #\. (string-ref num 0))
        (string "0" num)
        (if (char=? #\. (string-ref num (- (string-length num) 1)))
            (substring num 0 (- (string-length num) 1))
            num))))

(define (scheme-test-state scheme test)
  (let ((normal (find (lambda (el)
                        (and (eq? (first el) test)
                             (eq? (second el) scheme)))
                      data)))
    (if normal
        (third normal)
        (let ((prefix (string->symbol (car (burst-string (symbol->string scheme) #\- #f)))))
          (let ((other (find (lambda (el)
                               (and (eq? (first el) test)
                                    (eq? (second el) prefix)))
                             data)))
            (if other
                (third other)
                #f))))))
(define (ranks->sxml ranks)
  (map (lambda (r)
         `(span (@ (class ,(string-append "rank" (number->string r))))
                ,r))
       ranks))
(define (format-ranks ranks)
  `(div (@ (class "ranks"))
        (h4 "Test ranks (1-9)")
        (p "The following list shows how often a given Scheme was in the top 9. Every 1 means that this Scheme was first place, every 2 means second place, etc. Sorting is done in ascending order by summing the places (10 for every missing number).")
        ,@(map (lambda (el)
                 `(div (span (@ (class "name"))
                             ,(car el))
                       (span ,@(ranks->sxml (cdr el)))))
               ranks)))
(define (format-finished finished)
  (let* ((maximum (apply max (map second finished)))
         (width 300)
         (factor (truncate->exact (/ width maximum))))
    `(div (@ (class "finished"))
          (h4 "Tests finished")
          (p "Shows how many tests each Scheme actually finished.")
          (svg (@ (class "chart")
                  (width ,(number->string width))
                  (height ,(* 22 (length finished))))
               ,@(map (lambda (finished index)
                        (let* ((scheme (first finished))
                               (num (second finished))
                               (this-width (truncate->exact (* factor num)))
                               (color (list-ref rank-colors
                                                (- (- (length rank-colors) 1)
                                                   (truncate->exact (* (/ num maximum) (- (length rank-colors) 1)))))))
                          `(g (@ (transform ,(string-append "translate(0," (number->string (* 20 index)) ")")))
                              ;; (title "") ;; TODO
                              (rect (@ (width ,(nice-number this-width))
                                       (height 19)
                                       (style ,(string-append "fill:" color))))
                              (text (@ (x ,(nice-number (if (> this-width (/ width 2)) 5 (+ this-width 5))))
                                       (y 9.5)
                                       (dy ".35em"))
                                    ,(symbol->string scheme)
                                    " ("
                                    ,num
                                    ")"))))
                      finished
                      (iota (length finished)))))))
(define (format-tables)
  `(div (@ (class "speed-rank"))
        (h3 "Detailed results")
        (p "When showing " (strong "ranks") ", 1 is fastest.")
        (p "When showing " (strong "percentages") ", 100% is the slowest implementation.")
        (button (@ (id "rankpercent"))
                "Show Percentages")
        (table (thead (tr (th (@ (class "name")) "Test")
                          ,@(map (lambda (scheme) `(th
                                               (div (@ (class "scheme"))
                                                    (span (@ (title ,scheme))
                                                          ,scheme))))
                                 schemes)))
               (tbody ,@(map (lambda (test)
                               `(tr (td ,test)
                                    ,@(map (lambda (scheme)
                                             (let ((state (scheme-test-state scheme test)))
                                               (if (number? state)
                                                   (let* ((nr+count (scheme-position-in-test scheme test))
                                                          (nr (car nr+count))
                                                          (count (cdr nr+count))
                                                          (percentage (scheme-percentage-in-test scheme test))
                                                          (fast-percentage (scheme-fast-percentage-in-test scheme test)))
                                                     `(td (@ (class "box")
                                                             (style ,(string-append "background-color: " (scheme-position-color nr count) ";"))
                                                             (title ,(string-append (symbol->string scheme) " in test " (symbol->string test) html-nl
                                                                                    "rank " (number->string (+ nr 1)) html-nl
                                                                                    (string-append (nice-float percentage) "%") " of slowest" html-nl
                                                                                    (string-append (nice-float fast-percentage) "%") " of fastest")))
                                                          (span (@ (class "rank")) ,(+ nr 1))
                                                          (span (@ (class "percent") (style "display: none;")) ,(nice-float percentage) "%")

                                                          "\n"))
                                                   `(td (@ (class ,(string-append "box " (if state (symbol->string state) "unknown")))
                                                           (title ,(case state
                                                                     ((compileerror) "error when compiling")
                                                                     ((crashed) "crashed while running")
                                                                     ((ulimitkilled) "killed after time ran out")
                                                                     (else "no result, something went wrong"))))
                                                        ,(case state
                                                           ((compileerror)
                                                            `(span (@ (title "error when compiling")) (i (@ (class "fa fa-ban")))))
                                                           ((crashed)
                                                            `(span (@ (title "crashed while running")) (i (@ (class "fa fa-bolt")))))
                                                           ((ulimitkilled)
                                                            `(span (@ (title "killed after time ran out")) (i (@ (class "fa fa-hourglass"))))
                                                            )
                                                           (else
                                                            `(span (@ (title "no result, something went wrong")) (i (@ (class "fa fa-question-circle"))))))))))
                                           schemes)
                                    "\n"))
                             tests)))
        (script (*html* "
function showPercentages() {
document.showrank=false;
document.getElementById(\"rankpercent\").innerHTML=\"Show Ranks\";
var ps = document.getElementsByClassName('percent');
var rs = document.getElementsByClassName('rank');
for(var i=0; i<ps.length; i++) {
ps[i].style.display=\"inline\";
rs[i].style.display=\"none\";
}
};
function showRanks() {
document.showrank=true;
document.getElementById(\"rankpercent\").innerHTML=\"Show Percentages\";
var ps = document.getElementsByClassName('percent');
var rs = document.getElementsByClassName('rank');
for(var i=0; i<ps.length; i++) {
ps[i].style.display=\"none\";
rs[i].style.display=\"inline\";
}
};
document.showrank=false;
showRanks();
document.getElementById(\"rankpercent\").onclick = function(e) {
  if(document.showrank) {
  showPercentages();
  } else {
  showRanks();
  }
}
"))))

(define (format-date dt)
  (format #f "~a-~2,'0d-~2,'0d" (date-year dt) (date-month dt) (date-day dt)))
(define (format-tests)
  (format #t "<!DOCTYPE html>~%<html>
<head>
<meta charset=\"utf-8\"/>
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.6.3/css/font-awesome.min.css\">
<script>
</script>
<style>
.crashed, .compileerror, .ulimitkilled {
  color: #822;
  background-color: #fcc;
}
.unknown {
  background-color: #eeeeee;
  color: #aaaaaa;
}
th.name {
  width: 8em;
  height: 5em;
}
div.scheme {
  white-space: nowrap;
  text-align: left;
  width: 10em;
  transform-origin: left bottom;
  transform: translate(25px, 35px) rotate(315deg);
}
td.box {
  text-align: center;
  padding: 3px;
  margin: 4px;
  width: 2em;
  min-width: 2em;
  height: 1.7em;
  min-height: 1.7em;
}
table {
  table-layout: fixed;
  width: 100%;
  min-width: 1024px;
}
.sep {
  color: #666;
}
</style>
</head>
<body>
<h2>Scheme Benchmarks</h3>
<p>Based on the <a href=\"http://www.larcenists.org/benchmarksGenuineR6Linux.html\">Larceny benchmarks</a>. Code on <a href=\"http://www.github.com/ecraven/r7rs-benchmarks\">GitHub</a></p>
<p>Tests were run on an Intel(R) Core(TM) i3-N305 CPU @ 3.8GHz with 48GB of RAM by using the Arch Linux packages. No guarantees for any of the numbers. <a href=\"all.csv\">CSV raw data</a></p>
<p><small>Generated at ~a, <a href=\"mailto:r7rs-benchmarks@nexoid.at\">e-mail</a></small></p>
<h3>SAFE vs. UNSAFE optimizations</h3>
<p>The benchmark results on this page are collected in <em>safe</em> mode. This means various optimisations are turned <em>off</em> for each implementation. If you need code to run as fast as possible, look into that, the Readme on <a href=\"http://www.github.com/ecraven/r7rs-benchmarks\">GitHub</a> contains a few pointers for the different implementations.</p>
<p>Feel free to run the benchmarks yourself <em>without</em> safety turned on. You'll probably need to modify the <code>bench</code> file for that.</p>
<h3>Actual R7RS Support</h3>
<p>Many of these implementations do <strong>not</strong> fully implement R7RS, but instead there is a bit of \"shim\" code. You can find this by looking at <code>src/&lt;Name&gt;-prelude.scm</code> and <code>src/&lt;Name&gt;-postlude.scm</code>, to see which changes are necessary. Some changes are also made by the <code>bench</code> script, especially relating to <code>import</code>s.</p>
<p>However, all of these implementations are \"close enough\" to R7RS to run a non-trivial amount of code.</p>
<style>
p { margin: 0; }
div.testrun {
  display: block;
}
div.testruns {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
}
h4,h5 {
  padding: none; margin: none;
}
.chart rect {
  fill: steelblue;
}
p {
  padding-bottom: 10px;
}
.chart text {
  fill: black;
  font: 10px sans-serif;
//  text-anchor: end;
}
.ranks {
float:left;
padding: 1em;
max-width: 50em;
}
.finished {
float:left;
padding: 1em;
}
.speed-rank {
clear:both;
}
.ranks .name {
display: inline-block;
width: 10em;
text-align: right;
margin-right: 1em;
}
.rank1 { background-color: ~a; }
.rank2 { background-color: ~a; }
.rank3 { background-color: ~a; }
.rank4 { background-color: ~a; }
.rank5 { background-color: ~a; }
.rank6 { background-color: ~a; }
.rank7 { background-color: ~a; }
.rank8 { background-color: ~a; }
.rank9 { background-color: ~a; }

</style>"
          (format-date (time-utc->date (current-time)))
          (list-ref rank-colors 0)
          (list-ref rank-colors 1)
          (list-ref rank-colors 2)
          (list-ref rank-colors 3)
          (list-ref rank-colors 4)
          (list-ref rank-colors 5)
          (list-ref rank-colors 6)
          (list-ref rank-colors 7)
          (list-ref rank-colors 8))
  (sxml->xml (format-ranks tests-ranks) #f #f)
  (sxml->xml (format-finished tests-finished) #f #f)
  (sxml->xml (format-tables) #f #f)
  (format #t "<div style=\"clear: both\"></div><div class=\"testruns\">~%")
  (flush-output-port)
  (for-each (lambda (test)
              (sxml->xml (format-test test) #f #f)
              (newline)
              (flush-output-port))
            test-time)
  (format #t "</div></body></html>"))

(with-output-to-file "index.html"
  format-tests
  'replace)
