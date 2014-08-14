#lang info

(define collection "pkg-dep-draw")

(define deps '("base"
               "draw-lib"
               "gui-lib"))

(define build-deps '("racket-doc"
                     "scribble-lib"))

(define scribblings '(("pkg-dep-draw.scrbl")))
