#lang racket/base

(provide main)

(require racket/cmdline
         racket/list
         "analysis/lint-with-expand.rkt")

(define (main . args)
  (define --stdin #f)
  (command-line
   #:program "intellij/lint-with-expand"
   #:once-each
   [("--stdin") "Read from stdin" (set! --stdin #t)]
   #:args maybe-path
   (cond
     [--stdin
      (display-expand-errors (current-input-port))]
     [(empty? maybe-path)
      (raise-user-error "must supply either a path or use --stdin")]
     [else
      (define path (first maybe-path))
      (call-with-input-file path
        (lambda (in) (display-expand-errors in path)))])))
