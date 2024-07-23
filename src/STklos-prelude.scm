(define-syntax import
  (syntax-rules ()
    ((import foo bar ...)
     #t)))

;; By default STklos will recognize '|: as a keyword, not the symbol |:|.
;; so we disable keywords here (since they're not standard and not used in
;; the benchmarks anyway).
(keyword-colon-position 'none)

(define (this-scheme-implementation-name)
  (string-append "stklos-" (version)))
