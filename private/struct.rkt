#lang racket/base

(provide (struct-out pkg))

(struct pkg (deps build-deps) #:transparent)
