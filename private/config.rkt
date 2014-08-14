#lang racket/base
(require racket/draw)

(provide (all-defined-out))

(define FONT-SIZE 10)
(define SPACE-SIZE 16)
(define SEP-SIZE FONT-SIZE)
(define MARGIN FONT-SIZE)

(define face "Helvetica")

(define font (make-font #:face face #:size FONT-SIZE #:size-in-pixels? #t))
(define hilite-font (make-font #:face face #:size FONT-SIZE #:size-in-pixels? #t #:weight 'bold))
(define hilite-color (make-color 100 0 0))

(define dep-color "blue")
(define build-dep-color "purple")

;; Unscaled:
(define first-pen-width 2)
(define pen-width 1)
