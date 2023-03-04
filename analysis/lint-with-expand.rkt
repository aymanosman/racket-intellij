#lang racket/base

(provide display-expand-errors)

(require racket/list
         racket/string
         "lint-with-expand/find-matching-module-path.rkt")

(define (display-expand-errors in [source-name "<stdin>"])
  (with-handlers ([exn:fail:read?
                   (lambda (exn)
                     (write-lint-message 0 "read failed"))]
                  [exn:fail:filesystem:missing-module?
                   (lambda (exn)
                     (write-lint-message 0 "collection not found" (symbol->string (exn:fail:filesystem:missing-module-path exn))))])

    (define file-stx (read-syntax/lang source-name in))

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
                       (define stx (find-matching-module-path file-stx (exn:fail:filesystem:missing-module-path exn)))
                       (write-lint-message/syntax stx "cannot open module file"))]
                    [exn:fail?
                     (lambda (exn)
                       (eprintf "~a" (exn-message exn)))])
      (parameterize ([current-namespace (make-base-namespace)])
        (expand file-stx))))
  (void))

(define (write-lint-message/syntax stx message)
  (write-lint-message (syntax-position stx) message (syntax->datum stx)))

(define (write-lint-message offset message [detail ""])
  (printf "~s ~s ~a\n" offset message detail))

(define (read-syntax/lang source-name in)
  (parameterize ([read-accept-reader #t])
    (read-syntax source-name in)))
