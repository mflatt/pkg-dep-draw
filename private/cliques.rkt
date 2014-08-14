#lang racket/base
(require "struct.rkt")

(provide get-cliques
         find)

;; ----------------------------------------
;; union find

(define (find! reps key)
  (define rep-key (hash-ref reps key key))
  (if (equal? rep-key key)
      key
      (let ([rep-key (find! reps rep-key)])
        (hash-set! reps key rep-key)
        rep-key)))

(define (elect! reps key)
  (define rep-key (find! reps key))
  (unless (equal? rep-key key)
    (hash-set! reps rep-key key)
    (hash-set! reps key key)))

(define (union! reps a-key b-key)
  (define rep-a-key (find! reps a-key))
  (define rep-b-key (find! reps b-key))
  (unless (equal? rep-a-key rep-b-key)
    (hash-set! reps rep-b-key rep-a-key))
  rep-a-key)

;; Representative stays the same after `get-cliques` returns:
(define (find reps key)
  (find! reps key))

;; ----------------------------------------
;; cliques

(define (get-cliques pkgs)

  (define (all-deps pkg)
    (append
     (pkg-deps (hash-ref pkgs pkg))
     (pkg-build-deps (hash-ref pkgs pkg))))

  (define (get-reps all-deps)
    (define reps (make-hash))

    (let ()
      (define seen (make-hash))
      (let loop ([l (hash-keys pkgs)] [cycle-stack null])
        (unless (null? l)
          (let ([pkg (car l)])
            (cond
             [(member (find! reps pkg) cycle-stack)
              (for ([s (in-list (member (find! reps pkg) (reverse cycle-stack)))])
                (union! reps pkg s))
              (loop (cdr l) cycle-stack)]
             [(hash-ref seen pkg #f)
              (loop (cdr l) cycle-stack)]
             [else
              (hash-set! seen pkg #t)
              (loop (all-deps pkg) (cons pkg cycle-stack))
              (loop (cdr l) cycle-stack)])))))

    reps)

  ;; Cliques counting both non-build and build dependencies
  (define reps (get-reps all-deps))
  ;; Cliques counting only non-build dependencies:
  (define no-build-reps (get-reps (lambda (pkg) (pkg-deps (hash-ref pkgs pkg)))))

  ;; Mapping from clique representative to all members:
  (define rev-reps (make-hash))
  (for ([pkg (in-hash-keys pkgs)])
    (hash-update! rev-reps (find! reps pkg) (lambda (v) (cons pkg v)) null))

  ;; Compute each package's depth from the packages with no dependencies:
  (define depths (make-hash))
  (let ()
    (define (get-depth in-pkg)
      (define pkg (find! reps in-pkg))
      (cond
       [(hash-ref depths pkg #f)]
       [else
        (hash-set! depths pkg -1)
        (define total-deps (apply append
                                  (map all-deps (hash-ref rev-reps pkg))))
        (define depth (add1
                       (apply max -1 (map get-depth total-deps))))
        (hash-set! depths pkg depth)
        depth]))
    (for-each get-depth (hash-keys pkgs))
    (for ([pkg (in-hash-keys pkgs)])
      (hash-set! depths pkg (hash-ref depths (find! reps pkg)))))

  (define max-depth (apply max 0 (hash-values depths)))

  ;; Mapping from a depth to all packages at that depth (reverse of `depths`):
  (define at-depth
    (for/hash ([i (in-range 0 (add1 max-depth))])
      (values i
              (sort (for/list ([(k v) (in-hash depths)]
                               #:when (= v i))
                      k)
                    (lambda (a b)
                      (define ar (find! reps a))
                      (define br (find! reps b))
                      (cond
                       [(equal? ar br)
                        (define ar (find! no-build-reps a))
                        (define br (find! no-build-reps b))
                        (if (equal? ar br)
                            (string<? a b)
                            (string<? ar br))]
                       [else (string<? ar br)]))))))

  ;; Sanity check: no dependencies on a deeper package:
  (for ([pkg (in-hash-keys pkgs)])
    (define depth (hash-ref depths pkg))
    (for ([dep (in-list (append (all-deps pkg)))])
      (when ((hash-ref depths dep) . > . depth)
        (printf "inversion ~s -> ~s\n" pkg dep))))

  (values reps no-build-reps depths max-depth at-depth))
