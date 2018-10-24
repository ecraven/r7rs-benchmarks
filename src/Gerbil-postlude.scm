(import (only (gerbil core) gerbil-version-string string-split))
(define (this-scheme-implementation-name)
  (let* ((parts (string-split (gerbil-version-string) #\-))
         (version
          (if (> (length parts) 1) ; it's a dev version
            (string-append (car parts) "-" (cadr parts))
            (car parts))))
    (string-append "gerbil-" version)))
