(import (only (gerbil core) gerbil-version-string))
(define (this-scheme-implementation-name)
  (string-append "gerbil-" (gerbil-version-string)))
