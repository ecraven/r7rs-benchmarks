#!/bin/sh
# -*- wisp -*-
# graph-gmean -- plot the results with geometric mean into r7rs-plot.png
if echo "$@" | grep -- --help; then
  echo $0 [CSV-file] [scheme-impl ...]
  echo
  echo examples
  echo
  echo evaluate csv from ~/Downloads/all.csv
  echo $0 ~/Downloads/all.csv
  echo
  echo evaluate Chez, Loko, Guile, and Kawa against CSV downloaded from https://ecraven.github.io/r7rs-benchmarks/all.csv
  echo $0 ~/Downloads/all.csv chez loko guile kawa
  echo
  echo evaluate chez loko guile kawa against result.* in the current folder
  echo $0 chez loko guile kawa  
  exit 0
fi
# requires guile 3.0.10+, grep, sed, and gnuplot
GRAPH="r7rs-plot.png"
if test -f "$1"; then
  CSV="$1"
  shift
fi
if test -z "$1"; then
  SCHEMES="bigloo- bones- chez- chibi- chicken- cyclone- femtolisp- gambitc- gauche- gerbil- guile- ironscheme- kawa- larceny- loko- mit- mosh- picrin- racket- s7- s9fes- stklos- sagittarius- stalin- tr7- ypsilon-"
else
  SCHEMES="$@"
fi
TMPDIR=$(mktemp -d "/tmp/r7rs-benchgraph-XXXXXXXX")
function die {
  echo $1;
  exit 1;
}
if test -z "$CSV"; then
    grep -a -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > "$TMPDIR/all.csv" || die "Cannot grep data from results"
else
    cp "$CSV" "$TMPDIR/all.csv"
fi
for i in $SCHEMES; do
  guile -L "$(dirname "$(realpath "$0")")" --language=wisp -x .w -e '(graph-gmean)' -c '' "$TMPDIR/all.csv" "$i" "--for-gnuplot" >> $TMPDIR/r7rs-gmean.csv || die "ERROR: cannot summarize data $i"
done
gnuplot -e 'set term png size 1920,1920; set output "'$GRAPH'"; set ylabel "geometric mean slowdown vs. fastest"; set logscale y; set grid; unset xtics; plot "< LANG=C sort -gk2 '$TMPDIR'/r7rs-gmean.csv" using 0:2:3:4:xtic(1) with errorbars lw 3 ps 8 pt 5 title "geometric mean and stddev 68% range", "< LANG=C sort -gk2 '$TMPDIR'/r7rs-gmean.csv" using 0:2:($1) with labels left rotate offset first -0.15,character 1.6 notitle, "< LANG=C sort -gk2 '$TMPDIR'/r7rs-gmean.csv" using 0:2:(sprintf("%5.2f", $2)) with labels font "Mono,14" center offset first 0,character -2.2 notitle;'
exec echo $GRAPH

!#
;; Evaluate the benchmarks from ecraven at http://ecraven.github.io/r7rs-benchmarks/benchmark.html
;; Uses data from http://ecraven.github.io/r7rs-benchmarks/all.csv

define-module : graph-gmean
    . #:export : main

import : ice-9 rdelim
         srfi srfi-1
         only (srfi srfi-26) cut
         ice-9 pretty-print
         ice-9 optargs
         ice-9 i18n

define : read-csv port
    let loop : : lines '()
        if : eof-object? : peek-char port 
           reverse : map (λ (x) (string-split x #\,)) lines
           loop : cons (read-line port) lines

define : min-alist-by-test data-by-project
    let lp 
        : min-data '()
          data-by-project data-by-project
        if : null? data-by-project
           . min-data
           let*
             : proj : car : car data-by-project
               test : car : cdr : car data-by-project
               time : string->number : car : cdr : cdr : car data-by-project
               best : assoc-ref min-data test
             lp
                if : and time : or (not best) : < time best
                   assoc-set! min-data test time
                   . min-data
                cdr data-by-project

define : select-project-data data-by-project project
       define : notproj? datapoint
              not : string-prefix? project : car datapoint
       define : only-project data
              remove notproj? data
       map cdr : only-project data-by-project

define : get-multiples guile-data data-min-by-test
  let lp 
      : gd guile-data
        multiples-of-best '()
      if : null? gd
         remove (λ(x) (equal? #f x)) multiples-of-best
         let*
             : guile : string->number : car : cdr : car gd
               test : car : car gd
               multiple
                 if : not guile
                    . guile
                    / guile
                      or (assoc-ref data-min-by-test test) guile
             lp : cdr gd
                  if multiple
                     cons multiple multiples-of-best
                     . multiples-of-best


define : get-multiples-alist guile-data data-min-by-test
  let lp 
      : gd guile-data
        multiples-of-best '()
      if : null? gd
         remove (λ(x) (equal? #f x)) multiples-of-best
         let*
             : guile : string->number : car : cdr : car gd
               test : car : car gd
               multiple
                 if : not guile
                    cons test guile
                    cons test 
                      / guile
                        or (assoc-ref data-min-by-test test) guile
             lp : cdr gd
                  if multiple
                     cons multiple multiples-of-best
                     . multiples-of-best


define : help args
    format (current-error-port) "Usage: ~a [--help] [--csv] csv-file [project-prefix]\n" (car args)

define : csv-file args
    car : cdr args

define : remove-options args
    . "remove all options (starting with -) from the argument list. This ignores --."
    remove : λ(x) : string-prefix? "-" x
      . args

define : project-prefix args
    if : null? : cdr : cdr args
       . "guile"
       car : cdr : cdr args

define : geometric-mean mult
    ;; reference from python scipy.stats.gmean
    ;; geometric-mean '(1.0099785094926073 1.0023913275138778 1.007278530431698 1.0 1.0151278532049126 1.0241147352628563 1.0 1.010569433451955 1.0434718138402814 1.0 1.0 1.0 1.0850467380086724 1.0 1.0 1.0 1.0 1.0 1.013460400318081 1.0369464196049085 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0276886283570255 1.008484300792353 1.0 1.0 1.0054499614298174 1.0276891928360352 1.0 1.008044878261761 1.0079167383470071 1.0 1.015322834394769 1.0 1.0 1.0 1.000833236233333 1.0137968054031663 1.0 1.0709384846564785 1.0228005056088003 1.0076113765052213 1.0 1.0 1.0 1.0 1.0037700207244038 1.003870266995729 1.0 1.0 1.0)
    ;; => 1.008305573228468
    ;; see https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.gmean.html#scipy.stats.gmean
    if : null? mult
       . #f
       expt
          apply * mult
          / 1 : length mult

define : geometric-std mult
    ;; reference from python scipy.stats.gstd
    ;; geometric-std '(1.0099785094926073 1.0023913275138778 1.007278530431698 1.0 1.0151278532049126 1.0241147352628563 1.0 1.010569433451955 1.0434718138402814 1.0 1.0 1.0 1.0850467380086724 1.0 1.0 1.0 1.0 1.0 1.013460400318081 1.0369464196049085 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0276886283570255 1.008484300792353 1.0 1.0 1.0054499614298174 1.0276891928360352 1.0 1.008044878261761 1.0079167383470071 1.0 1.015322834394769 1.0 1.0 1.0 1.000833236233333 1.0137968054031663 1.0 1.0709384846564785 1.0228005056088003 1.0076113765052213 1.0 1.0 1.0 1.0 1.0037700207244038 1.003870266995729 1.0 1.0 1.0)
    ;; => 1.0164423836362904
    define len : length mult
    define d 1 ;; degrees of freedom; using default of 1
    if : null? mult
       nan ;; not applicable
       ;; mean of the natural logarithms of the observations
       let : : ȳ : * (/ 1 len) : apply + : map log mult
         exp
           sqrt
             * {1 / {len - d}}
               apply +
                 map : cut expt <> 2
                   map : cut - <> ȳ
                     map log mult

define : format-geometric-mean-std g s_g
    format #f "~a (~a to ~a)"
         . g {g / s_g} {g * s_g}

define : format-title project-prefix
    while : string-suffix? "-" project-prefix
      set! project-prefix : string-drop-right project-prefix 1
    string-locale-titlecase project-prefix

define : main args
    when : member "--help" args 
         help args
         exit 0
    let*
      : port : open-input-file : csv-file args
        data-by-project : read-csv port
        data-min-by-test : min-alist-by-test data-by-project
        guile-data : select-project-data data-by-project : project-prefix args
        err : current-error-port
      when : member "--csv" args
          ; display "test slowdown\n"
          map : λ (x) : apply format #t "~a ~a\n" : list (car x) (cdr x)            
                  get-multiples-alist guile-data data-min-by-test          
          format err "total ~a\n"
              if : null? : get-multiples guile-data data-min-by-test
                  . #f
                  expt
                      apply * : get-multiples guile-data data-min-by-test
                      / 1 : length : get-multiples guile-data data-min-by-test
          exit 0
          
      format err "=== Best times ===\n\n"
      pretty-print : sort data-min-by-test (λ (x y) (string<? (car x) (car y)))
        . err
      newline err
      format err "=== ~a times ===\n\n" : string-locale-titlecase : project-prefix args
      pretty-print : sort guile-data (λ (x y) (string<? (car x) (car y)))
        . err
      newline err
      format err "=== ~a slowdown ===\n\n" : string-locale-titlecase : project-prefix args
      pretty-print
        sort : get-multiples-alist guile-data data-min-by-test
             λ (x y) (string<? (car x) (car y))
        . err
      newline err
      format err "=== ~a Geometric Mean slowdown (successful tests / total tests) ===\n\n" : string-locale-titlecase : project-prefix args
      let*
          : data : map inexact->exact : get-multiples guile-data data-min-by-test
            g : geometric-mean data
            s_g : geometric-std data
            gstr : format-geometric-mean-std g s_g
            title : format-title : project-prefix args
            success-count : length : remove (λ(x) (equal? #f (string->number (car (cdr x))))) guile-data
            total-count : length guile-data
          format err "~a (~a / ~a)\n"
             . g
             length : remove (λ(x) (equal? #f (string->number (car (cdr x))))) guile-data
             length guile-data
          if : member "--for-gnuplot" args
             format #t "~a ~a ~a ~a ~a ~a\n"
               . title g {g / s_g} {g * s_g} success-count total-count
             format #t "~a -- ~a -- (~a / ~a)\n"
               . gstr title success-count total-count
