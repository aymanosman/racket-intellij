#lang racket/base

(provide main)

(require racket/cmdline
         racket/list)

(define (expand-for-unbound-identifier-errors in [file-name "<stdin>"])
  (with-handlers ([exn:fail:syntax:unbound?
                   (lambda (exn)
                     (define exprs (exn:fail:syntax-exprs exn))
                     (cond
                       [(empty? exprs)
                        (void)]
                       [else
                        (define stx (first exprs))
                        (printf "~a ~a\n" (syntax-position stx) (syntax-e stx))])
                     (exit 0))]
                  [exn:fail? (lambda (exn)
                               (eprintf "~a" (exn-message exn))
                               (exit 0))])
    (parameterize ([read-accept-reader #t])
      (parameterize ([current-namespace (make-base-namespace)])
        (expand (read-syntax file-name in))))
    (exit 0)))

(define (main . args)
  (define --stdin #f)
  (command-line
   #:program "intellij/unbound-identifiers"
   #:once-each
   [("--stdin") "Read from stdin" (set! --stdin #t)]
   #:args maybe-path
   (cond
     [--stdin
      (expand-for-unbound-identifier-errors (current-input-port))]
     [(empty? maybe-path)
      (raise-user-error "must supply either a path or use --stdin")]
     [else
      (define path (first maybe-path))
      (call-with-input-file path
        (lambda (in) (expand-for-unbound-identifier-errors in path)))])))
