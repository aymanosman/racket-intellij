#lang racket/base

(provide main)

(require racket/cmdline
         "analysis/unused-requires.rkt")

(define (main . args)
  (define --stdin #f)
  (command-line
   #:program "intellij/unused-requires"
   #:once-each
   [("--stdin") "Read from stdin (path is still required)" (set! --stdin #t)]
   #:args (path)
   (cond
     [--stdin
      (display-unused-requires (current-input-port) path)]
     [else
      (call-with-input-file path
        (lambda (in)          
          (display-unused-requires in path)))])))