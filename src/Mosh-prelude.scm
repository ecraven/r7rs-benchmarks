(import (mosh config))
(define (this-scheme-implementation-name)
  (string-append "mosh-" (get-config "version")))
