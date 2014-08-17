#lang racket/base
(require racket/class
         racket/draw
         racket/string
         "struct.rkt"
         "config.rkt"
         "cliques.rkt")

(provide pkg-extent draw-graph)

;; Compute a package-name display size, converting "-" to line breaks:
(define (pkg-extent dc pkg font)
  (define l (string-split pkg #rx"-"))
  (define ls (for/list ([s (in-list l)])
               (define-values (w h d a) (send dc get-text-extent s font #t))
               (cons w h)))
  (values (apply max (map car ls))
          (apply + (map cdr ls))
          0 0))

;; Draw a package name, converting "-" to line breaks:
(define (draw-pkg dc pkg font x y p-w p-h)
  (define l (string-split pkg #rx"-"))
  (for/fold ([y y]) ([s (in-list l)])
    (define-values (w h d a) (send dc get-text-extent s font #t))
    (send dc draw-text s (+ x (/ (- p-w w) 2)) y #t)
    (+ y h)))

(define (draw-graph dc
                    #:pkgs pkgs
                    #:invert-pkgs invert-pkgs
                    #:pkg-bounds pkg-bounds
                    #:at-depth at-depth
                    #:reps reps
                    #:no-build-reps no-build-reps
                    #:total-w total-w 
                    #:total-h total-h
                    #:view-scale view-scale
                    #:invert? invert?
                    #:all-deps? all-deps?
                    #:trans-deps? [trans-deps? #t]
                    #:hilites hilites)
  (send dc set-font font)
  (send dc set-scale view-scale view-scale)

  (define first-dep-pen (make-pen #:color dep-color #:width (/ first-pen-width view-scale)))
  (define first-build-dep-pen (make-pen #:color build-dep-color #:width (/ first-pen-width view-scale)))
  (define dep-pen (make-pen #:color dep-color #:width (/ pen-width view-scale)))
  (define build-dep-pen (make-pen #:color build-dep-color #:width (/ pen-width view-scale)))
  (define dep-brush (make-brush #:color dep-color))
  (define build-dep-brush (make-brush #:color build-dep-color))
  
  ;; Connect packages that are in a clique
  (send dc set-pen (make-pen #:style 'transparent))
  (for ([pkgs (in-hash-values at-depth)])
    (let loop ([pkgs pkgs])
      (cond
       [(null? pkgs) (void)]
       [(null? (cdr pkgs)) (void)]
       [else
        (when (equal? (find reps (car pkgs)) (find reps (cadr pkgs)))
          (send dc set-brush (if (equal? (find no-build-reps (car pkgs))
                                         (find no-build-reps (cadr pkgs)))
                                 dep-brush
                                 build-dep-brush))
          (define bounds (hash-ref pkg-bounds (cadr pkgs)))
          (define I 3)
          (define sz (- SEP-SIZE (* 2 I)))
          (send dc draw-rectangle
                (+ I (- (car bounds) SEP-SIZE)) (+ (cadr bounds)
                                                   (/ (- (cadddr bounds) sz) 2))
                (- SEP-SIZE (* 2 I)) sz))
        (loop (cdr pkgs))])))
  (send dc set-brush (make-brush #:style 'transparent))

  (for ([pkg (in-hash-keys pkgs)])
    (define bounds (hash-ref pkg-bounds pkg))
    (define p (hash-ref (if invert? invert-pkgs pkgs) pkg))
    (when (hash-ref hilites pkg #f)
      (define here? (zero? (hash-ref hilites pkg)))
      (send dc set-alpha (/ 1 (expt 1.5 (hash-ref hilites pkg))))
      (for ([deps (list (pkg-deps p) (if all-deps? (pkg-build-deps p) null))]
            [pen (if here?
                     (list first-dep-pen first-build-dep-pen)
                     (list dep-pen build-dep-pen))]
            #:when (or here? trans-deps?))
        (for ([dep (in-list deps)])
          (define dest (hash-ref pkg-bounds dep))
          (define-values (from to)
            (if invert?
                (values dest bounds)
                (values bounds dest)))
          (send dc set-pen pen)
          (cond
           [(equal? (find reps dep) (find reps pkg))
            (define x1 (+ (car from) (/ (caddr from) 2)))
            (define x3 (if ((car to) . < . (car from))
                           (+ (car to) (caddr to))
                           (car to)))
            (send dc draw-spline
                  x1 (cadr from)
                  (/ (+ x1 x3) 2) 
                  (- (min (cadr to) (cadr from))
                     (* SPACE-SIZE (+ 1 (/ (abs (- x1 x3)) total-w))))
                  x3
                  (cadr to))]
           [else
            (send dc draw-line
                  (+ (car from) (/ (caddr from) 2)) (+ (cadr from) (cadddr from))
                  (+ (car to) (/ (caddr to) 2)) (cadr to))])))
      (send dc set-alpha 1)))
  (for ([pkg (in-hash-keys pkgs)])
    (define bounds (hash-ref pkg-bounds pkg))
    (define maybe-on? (hash-ref hilites pkg #f))
    (define on? (or (and trans-deps? maybe-on?) (and maybe-on? (<= maybe-on? 1))))
    (define draw-font (if on? hilite-font font))
    (send dc set-font draw-font)
    (send dc set-text-foreground (if on? hilite-color "black"))
    (define-values (w h d a) (pkg-extent dc pkg draw-font))
    (draw-pkg dc pkg draw-font
              (+ (car bounds) (/ (- (caddr bounds) w) 2))
              (+ (cadr bounds) (/ (- (cadddr bounds) h) 2))
              w h)))
