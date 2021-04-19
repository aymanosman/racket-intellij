#lang racket/base

(provide find-matching-module-path)

(require racket/list
         syntax/parse)

(define (find-matching-module-path file-stx module-path)
  (or
   (for/or ([stx (in-module-paths file-stx)]
            #:when (module-path=? (syntax-e stx) module-path))
     stx)
   file-stx))

(define (in-module-paths file-stx)
  (syntax-parse file-stx
    #:datum-literals (module)
    [(module name lang (#%module-begin form* ...))
     (flatten
      (map syntax->list
           (filter-map (lambda (stx)
                         (syntax-parse stx
                           #:datum-literals (require)
                           [(require module-path* ...) #'(module-path* ...)]
                           [_ #f]))
                       (syntax->list #'(form* ...)))))]))


(define (module-path=? p q)
  (cond
    [(string? p)
     (equal? (build-path (current-directory) p) q)]
    [else
     (equal? p q)]))
