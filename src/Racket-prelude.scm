#lang r7rs
(import (scheme base) (only (racket base) version))
(define (this-scheme-implementation-name)
  (string-append "racket-" (version) "/r7rs"))
