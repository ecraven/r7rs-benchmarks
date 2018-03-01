(import (only (gerbil core) gerbil-version-string))
(define (this-scheme-implementation-name)
  (string-append "gerbil-" (car (string-split (gerbil-version-string) #\-))))
