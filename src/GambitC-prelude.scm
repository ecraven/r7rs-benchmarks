(declare (standard-bindings)   ;; builtin functions like + will not be redefined
         (extended-bindings)   ;; Gambit functions like fixnum? will not be redefined
         (block)               ;; user-defined functions not set! in the file will not be redefined
         ;; (not safe)         ;; do not use unsafe optimizations
         )

(define (this-scheme-implementation-name) (string-append "gambitc-" (system-version-string)))
