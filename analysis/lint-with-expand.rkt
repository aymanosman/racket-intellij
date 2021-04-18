#lang racket/base

(provide display-expand-errors)

(require racket/list
         racket/string)

(define (display-expand-errors in [file-name "<stdin>"])
  (with-handlers ([exn:fail:syntax:unbound?
                   (lambda (exn)
                     (define exprs (exn:fail:syntax-exprs exn))
                     (cond
                       [(empty? exprs)
                        (void)]
                       [else
                        (write-lint-message (first exprs) "unbound identifier")]))]
                  [exn:fail:syntax?
                   (lambda (exn)
                     (define exprs (exn:fail:syntax-exprs exn))
                     (cond
                       [(empty? exprs)
                        (void)]
                       [else
                        (cond
                          [(string-contains? (exn-message exn) "duplicate binding")
                           (write-lint-message (first exprs) "duplicate binding")]
                          [(string-contains? (exn-message exn) "bad syntax")
                           (write-lint-message (first exprs) "bad syntax")]
                          [else
                           (write-lint-message (first exprs) "error detected")])]))]
                  [exn:fail?
                   (lambda (exn)
                     (eprintf "~a" (exn-message exn)))])
    (parameterize ([read-accept-reader #t])
      (parameterize ([current-namespace (make-base-namespace)])
        (expand (read-syntax file-name in))))
    (void)))

(define (write-lint-message stx message)
  (printf "~a ~s ~a\n" (syntax-position stx) message (syntax->datum stx)))
