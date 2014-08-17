#lang racket/base
(require racket/runtime-path
         racket/class
         racket/draw
         racket/format
         "private/struct.rkt"
         "private/get-pkgs.rkt"
         "private/cliques.rkt"
         "private/config.rkt"
         "private/draw.rkt"
         "private/closure.rkt")

(define-runtime-module-path gui "private/gui.rkt")

(provide pkg-dep-draw)

(module+ main
  (require racket/cmdline)

  (define srcs null)
  (define no-build? #f)
  (define no-build-lines? #f)
  (define no-trans-lines? #f)
  (define invert? #f)
  (define quiet? #f)
  (define dest-file #f)
  (define dest-format 'png)
  (define init-view-scale 1)
  (define select-pkgs null)

  (define root-packages
    (command-line
     #:multi
     [("--select") pkg "Show dependencies from <pkg>"
      (set! select-pkgs (cons pkg select-pkgs))]
     #:multi
     [("--catalog") catalog "Look for packages in <catalog>"
      (set! srcs (cons (cons 'catalog catalog) srcs))]
     [("--dir") dir "Look for packages in <dir>"
      (set! srcs (cons (cons 'dir dir) srcs))]
     #:once-each
     [("--no-build") "Ignore build dependencies"
      (set! no-build? #t)]
     [("--no-build-lines") "Suppress build-dependency lines"
      (set! no-build-lines? #t)]
     [("--no-trans-lines") "Suppress transitive lines"
      (set! no-trans-lines? #t)]
     [("--reverse") "Follow dependencies backwards"
      (set! invert? #t)]
     [("--scale") scale "Initial drawing scale"
      (define n (string->number scale))
      (unless (and (rational? n) (positive? n))
        (raise-user-error 'pkg-dep-draw
                          (~a "bad scale\n"
                              "  given: ~a")
                          scale))
      (set! init-view-scale n)]
     [("-o") file "Draw graph to <file>"
      (set! dest-file file)
      (set! dest-format
            (cond
             [(regexp-match? #rx"[.]png$" file) 'png]
             [(regexp-match? #rx"[.]pdf$" file) 'pdf]
             [else (raise-user-error 'pkg-dep-draw
                                     (~a "unknown destination extension\n"
                                         "  path: ~a\n"
                                         "  supported extensions: .png .pdf")
                                     file)]))]
     [("--quiet" "-q") "Suppress hints and notes"
      (set! quiet? #t)]
     #:args
     root-pkg
     root-pkg))

  (pkg-dep-draw #:root-pkgs root-packages
                #:select-pkgs select-pkgs
                #:srcs srcs
                #:no-build? no-build?
                #:no-build-lines? no-build-lines?
                #:no-trans-lines? no-trans-lines?
                #:invert? invert?
                #:quiet quiet?
                #:dest-file dest-file
                #:dest-format dest-format
                #:scale init-view-scale))

;; ----------------------------------------

(define (pkg-dep-draw #:root-pkgs [root-packages null]
                      #:select-pkgs [select-pkgs null]
                      #:srcs [srcs null]
                      #:no-build? [no-build? #f]
                      #:no-build-lines? [no-build-lines? #f]
                      #:no-trans-lines? [no-trans-lines? #f]
                      #:invert? [invert? #f]
                      #:quiet [quiet? #t]
                      #:dest-file [dest-file #f]
                      #:dest-format [dest-format 'png]
                      #:scale [init-view-scale 1])

  (define-values (pkgs invert-pkgs)
    (get-pkgs #:srcs srcs
              #:roots root-packages
              #:quiet quiet?
              #:no-build? no-build?))

  (define-values (reps no-build-reps depths max-depth at-depth)
    (get-cliques pkgs))

  ;; ----------------------------------------

  ;; For sizing text:
  (define dc (send (make-bitmap 1 1) make-dc))

  ;; Mapping from depths to display heights:
  (define line-heights (for/hash ([i (in-range 0 (add1 max-depth))])
                         (define pkgs (hash-ref at-depth i))
                         (values i
                                 (for/fold ([l-h 0]) ([pkg (in-list pkgs)])
                                   (define-values (w h d a) (pkg-extent dc pkg font))
                                   (max h l-h)))))

  (define total-h (+ (* max-depth SPACE-SIZE)
                     (apply + (hash-values line-heights))
                     (* 2 MARGIN)))

  ;; Mapping from depths to display widths:
  (define line-widths (for/hash ([i (in-range 0 (add1 max-depth))])
                        (define pkgs (hash-ref at-depth i))
                        (values i
                                (+ (* (sub1 (length pkgs)) SEP-SIZE)
                                   (for/fold ([l-w 0]) ([pkg (in-list pkgs)])
                                     (define-values (w h d a) (pkg-extent dc pkg font))
                                     (+ w l-w))))))

  (define total-w (+ (* 2 MARGIN)
                     (apply max 0 (hash-values line-widths))))

  ;; Mapping from package name to display bounding box:
  (define pkg-bounds
    (let ()
      (define-values (pkg-bounds ignored-y)
        (for/fold ([ht (hash)] [y (- total-h MARGIN)]) ([i (in-range 0 (add1 max-depth))])
          (define pkgs (hash-ref at-depth i))
          (define l-h (hash-ref line-heights i))
          (define l-w (hash-ref line-widths i))
          (define x (/ (- total-w l-w) 2))
          (define-values (new-ht new-x)
            (for/fold ([ht ht] [x x]) ([pkg (in-list pkgs)])
              (define-values (w h d a) (pkg-extent dc pkg font))
              (values (hash-set ht pkg (list x (+ y (- l-h) (/ (- l-h h) 2)) w h))
                      (+ x SEP-SIZE w))))
          (values new-ht (- y l-h SPACE-SIZE))))
      pkg-bounds))

  ;; ----------------------------------------

  (cond
   [dest-file
    (define (->int n) (ceiling (inexact->exact n)))
    (define dc
      (case dest-format
        [(png)
         (define bm (make-bitmap (->int (* init-view-scale total-w))
                                 (->int (* init-view-scale total-h))))
         (define dc (send bm make-dc))
         (send dc set-smoothing 'smoothed)
         dc]
        [(pdf)
         (define pss (current-ps-setup))
         (send pss set-file dest-file)
         (send pss set-scaling 1.0 1.0)
         (define dc (new pdf-dc%
                         [interactive #f]
                         [width (* init-view-scale total-w)]
                         [height (* init-view-scale total-h)]))
         (send dc start-doc "graph")
         (send dc start-page)
         dc]
        [else (error "unknown format")]))
    (draw-graph dc
                #:pkgs pkgs
                #:invert-pkgs invert-pkgs
                #:pkg-bounds pkg-bounds
                #:at-depth at-depth
                #:reps reps
                #:no-build-reps no-build-reps
                #:total-w total-w 
                #:total-h total-h
                #:view-scale init-view-scale
                #:invert? invert?
                #:all-deps? (not no-build-lines?)
                #:trans-deps? (not no-trans-lines?)
                #:hilites (compute-closure
                           (if invert? invert-pkgs pkgs)
                           (let ([select-pkgs
                                  (if (null? select-pkgs)
                                      root-packages
                                      select-pkgs)])
                             (if (null? select-pkgs)
                                 (for/list ([(k v) (in-hash invert-pkgs)]
                                            #:when (and (null? (pkg-deps v))
                                                        (null? (pkg-build-deps v))))
                                   k)
                                 select-pkgs))
                           (not no-build-lines?)))
    (case dest-format
      [(png)
       (void (send (send dc get-bitmap) save-file dest-file 'png))]
      [(pdf)
       (send dc end-page)
       (send dc end-doc)])]
   [else
    ((dynamic-require gui 'make-gui)
     #:pkgs pkgs
     #:invert-pkgs invert-pkgs
     #:pkg-bounds pkg-bounds
     #:at-depth at-depth
     #:reps reps
     #:no-build-reps no-build-reps
     #:total-w total-w 
     #:total-h total-h
     #:view-scale init-view-scale
     #:all-deps? (not no-build-lines?)
     #:trans-deps? (not no-trans-lines?)
     #:invert? invert?
     #:select-packages select-pkgs)]))
