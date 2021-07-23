(declare (usual-integrations))
(define old-values values)
(define (values . objects)
  (if (= (length objects) 1)
      (car objects)
      (apply old-values objects)))
(define-syntax import
  (syntax-rules ()
    ((import stuff ...)
     (begin) ;; do nothing
     )))

(define (this-scheme-implementation-name) (string-append "mit-" (get-subsystem-version-string "Release")))
