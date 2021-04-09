#lang racket/base

(provide find-requires)

(require racket/list
         syntax/parse)

(define (require-form? stx)
  (syntax-parse stx
    #:literals (#%require)
    [(#%require module-path)
     #'module-path]
    [_
     #f]))

(define (require-forms path)
  (call-with-input-file path
    (lambda (in)
      (define stx
        (parameterize ([read-accept-reader #t])
          (parameterize ([current-namespace (make-base-namespace)])
            (expand (read-syntax path in)))))

      (syntax-parse stx
        #:literals (module)
        [(module name lang (#%module-begin form* ...))
         (filter-map require-form? (syntax->list #'(form* ...)))]))))

(define (find-requires path)
  (for/hash ([stx (require-forms path)])
    (values (symbol->string (syntax-e stx)) stx)))
