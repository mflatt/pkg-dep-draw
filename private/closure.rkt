#lang racket/base
(require "struct.rkt")

(provide compute-closure)

(define (compute-closure pkgs hit-pkgs all-deps?)
  (define hilites (make-hash))
  (let loop ([hit-pkgs hit-pkgs] [dist 0])
    (for ([pkg (in-list hit-pkgs)])
      (hash-set! hilites pkg dist))
    (define deps (apply
                  append
                  (for/list ([pkg (in-list hit-pkgs)])
                    (let ([p (hash-ref pkgs pkg)])
                      (append (if all-deps? (pkg-build-deps p) null)
                              (pkg-deps p))))))
    (define new-deps
      (for/list ([dep (in-list deps)]
                 #:unless (hash-ref hilites dep #f))
        dep))
    (unless (null? new-deps)
      (loop new-deps (add1 dist))))
  hilites)
