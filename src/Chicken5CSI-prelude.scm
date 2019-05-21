(import (only (chicken platform) chicken-version))
(define (this-scheme-implementation-name) (string-append "chicken5csi-" (chicken-version)))
