#lang racket/base

(provide main
         display-unused-requires)

(require racket/match
         racket/cmdline
         "find-requires.rkt")

(require macro-debugger/analysis/private/util
         macro-debugger/analysis/private/nom-use-alg
         macro-debugger/analysis/private/get-references)

;; Copied from macro-debugger package
(define (analyze-requires mod-path)
  (let-values ([(compiled deriv) (get-module-code/trace mod-path)])
    (nom-use-alg (deriv->refs deriv) compiled)))

(define (display-unused-requires mod-path)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (eprintf "~a" (exn-message exn)))])
    (define require=>syntax (find-requires mod-path))
    (for ([entry (in-list (analyze-requires mod-path))])
      (match entry
        [(list 'drop mpi phase)
         (define key (mpi->key mpi))
         (define stx (hash-ref require=>syntax key))
         (printf "~a ~a\n" key (syntax-position stx))]
        [_
         (void)]))))

(define (main . args)
  (command-line
   #:program "intellij/unused-requires"
   #:args (path)
   (display-unused-requires path)))
