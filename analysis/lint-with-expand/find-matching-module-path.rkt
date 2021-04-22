#lang racket/base

(provide find-matching-module-path)

(require racket/list
         racket/bool
         syntax/parse)

(define (find-matching-module-path file-stx module-path)
  (or
   (for/or ([stx (in-module-paths file-stx)]
            #:when (module-path=? (syntax-e stx) module-path))
     stx)
   (first-require-form file-stx)
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
                           [(require module-path* ...)
                            #'(module-path* ...)]
                           [_ #f]))
                       (syntax->list #'(form* ...)))))]))


(define (module-path=? p q)
  (cond
    [(string? p)
     (equal? (simplify-path (build-path (current-directory) p)) q)]
    [else
     (equal? p q)]))

(define (first-require-form stx)
  (syntax-parse stx
    #:datum-literals (module)
    [(module name lang (#%module-begin form* ...))
     (for/or ([require-stx (syntax-case stx ()
                             [(module name lang (#%module-begin form* ...))
                              (filter-map (lambda (stx)
                                            (syntax-case stx ()
                                              [(head module-path* ...)
                                               (symbol=? (syntax-e #'head) 'require)
                                               stx]
                                              [_ #f]))
                                          (syntax->list #'(form* ...)))])])
       require-stx)]))
