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
                        (write-lint-message/syntax (first exprs) "unbound identifier")]))]
                  [exn:fail:syntax?
                   (lambda (exn)
                     (define exprs (exn:fail:syntax-exprs exn))
                     (cond
                       [(empty? exprs)
                        (void)]
                       [else
                        (cond
                          [(string-contains? (exn-message exn) "duplicate binding")
                           (write-lint-message/syntax (first exprs) "duplicate binding")]
                          [(string-contains? (exn-message exn) "bad syntax")
                           (write-lint-message/syntax (first exprs) "bad syntax")]
                          [else
                           (write-lint-message/syntax (first exprs) "error detected")])]))]
                  [exn:fail:filesystem:missing-module?
                   (lambda (exn)
                     (define path (exn:fail:filesystem:missing-module-path exn))
                     (write-lint-message 0 (format "cannot open module file ~a" (path->string path))))]
                  [exn:fail?
                   (lambda (exn)
                     (println exn)
                     (eprintf "~a" (exn-message exn)))])
    (parameterize ([read-accept-reader #t])
      (parameterize ([current-namespace (make-base-namespace)])
        (expand (read-syntax file-name in))))
    (void)))

(define (write-lint-message/syntax stx message)
  (write-lint-message (syntax-position stx) message (syntax->datum stx)))

(define (write-lint-message offset message [detail ""])
  (printf "~s ~s ~a\n" offset message detail))
