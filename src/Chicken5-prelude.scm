(import (only (chicken platform) chicken-version))
(define (this-scheme-implementation-name) (string-append "chicken5-" (chicken-version)))
