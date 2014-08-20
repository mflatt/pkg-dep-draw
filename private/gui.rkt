#lang racket/base
(require racket/gui/base
         racket/class
         "struct.rkt"
         "config.rkt"
         "draw.rkt"
         "closure.rkt")

(provide make-gui)

(define (->int n) (ceiling (inexact->exact n)))

(define (make-gui #:pkgs pkgs
                  #:invert-pkgs invert-pkgs
                  #:pkg-bounds pkg-bounds
                  #:at-depth at-depth
                  #:reps reps
                  #:no-build-reps no-build-reps
                  #:total-w total-w 
                  #:total-h total-h
                  #:view-scale [init-view-scale 1]
                  #:all-deps? [all-deps? #t]
                  #:trans-deps? [trans-deps? #t]
                  #:invert? [invert? #f]
                  #:select-packages [select-pkgs null])

  ;; GUI selection state:
  (define pin? (not (null? select-pkgs)))
  (define hover select-pkgs)
  (define hilites (if (null? select-pkgs)
                      (hash)
                      (compute-closure (if invert? invert-pkgs pkgs)
                                       select-pkgs
                                       all-deps?)))

  (define-values (screen-w screen-h) (get-display-size))

  (define f (new frame%
                 [label "Packages"]
                 [width (- screen-w 100)]
                 [height (- screen-h 100)]))

  (define panel (new vertical-panel%
                     [parent f]
                     [alignment '(left center)]))

  (define refresh-offscreen? #t)
  (define offscreen #f)
  (define offscreen-dc #f)
  (define view-scale init-view-scale)

  (define c (new (class canvas%
                   (super-new)
                   (inherit refresh
                            get-view-start
                            get-client-size
                            scroll
                            init-auto-scrollbars)
                   (define queued-refresh? #f)
                   (define/private (low-priority-refresh)
                     (unless queued-refresh?
                       (set! queued-refresh? #t)
                       (queue-callback
                        (lambda ()
                          (set! queued-refresh? #f)
                          (refresh))
                        #f)))
                   (define/private (adjust-scroll dx dy)
                     (define-values (w h) (get-client-size))
                     (define-values (x y) (get-view-start))
                     (define adj-w (- (* view-scale total-w) w))
                     (define adj-h (- (* view-scale total-h) h))
                     (define sx (min 1 (max 0 (+ (* dx 1/20) (/ x adj-w)))))
                     (define sy (min 1 (max 0 (+ (* dy 1/20) (/ y adj-h)))))
                     (define tx (and (not (= x (* sx adj-w)))
                                     sx))
                     (define ty (and (not (= y (* sy adj-h)))
                                     sy))
                     (when (or tx ty)
                       (scroll tx ty)))
                   (define/private (adjust-scale ds)
                     (define scale (max 1/5 (+ view-scale ds)))
                     (unless (= scale view-scale)
                       (set! view-scale scale)
                       (set! offscreen #f)
                       (set! refresh-offscreen? #t)
                       (low-priority-refresh)
                       (define-values (w h) (get-client-size))
                       (define-values (x y) (get-view-start))
                       (define adj-w (- (* view-scale total-w) w))
                       (define adj-h (- (* view-scale total-h) h))
                       (init-auto-scrollbars (->int (* scale total-w))
                                             (->int (* scale total-h))
                                             (min 1 (max 0 (/ x adj-w)))
                                             (min 1 (max 0 (/ y adj-h))))))
                   (define/override (on-char e)
                     (for/or ([key-code (list (send e get-key-code)
                                              (send e get-other-shift-key-code)
                                              (send e get-other-altgr-key-code))])
                       (case key-code
                         [(wheel-up up) (adjust-scroll 0 -1) #t]
                         [(wheel-down down) (adjust-scroll 0 1) #t]
                         [(wheel-left left) (adjust-scroll -1 0) #t]
                         [(wheel-right right) (adjust-scroll 1 0) #t]
                         [(#\+) (adjust-scale 1/10) #t]
                         [(#\-) (adjust-scale -1/10) #t]
                         [else #f])))
                   (define/override (on-event e)
                     (define-values (dx dy) (get-view-start))
                     (define x (/ (+ dx (send e get-x)) view-scale))
                     (define y (/ (+ dy (send e get-y)) view-scale))
                     (define hit-pkgs
                       (if (and pin?
                                (not (send e button-down?)))
                           hover
                           (for/or ([(pkg bounds) (in-hash pkg-bounds)])
                             (and (x . >= . (car bounds))
                                  (y . >= . (cadr bounds))
                                  (x . <= . (+ (car bounds) (caddr bounds)))
                                  (y . <= . (+ (cadr bounds) (cadddr bounds)))
                                  (list pkg)))))
                     (when (send e button-down?)
                       (set! pin? (and hit-pkgs #t)))
                     (unless (equal? hover hit-pkgs)
                       (set! hover hit-pkgs)
                       (reset-hilites)))
                   (define/public (reset-hilites)
                     (define hit-pkgs hover)
                     (set! hilites (hash))
                     (when hit-pkgs
                       (define all-deps? (send build-deps-checkbox get-value))
                       (define invert? (send invert-checkbox get-value))
                       (set! hilites (compute-closure (if invert? invert-pkgs pkgs)
                                                      hit-pkgs
                                                      all-deps?)))
                     (set! refresh-offscreen? #t)
                     (low-priority-refresh)))
                 [parent panel]
                 [style '(hscroll vscroll)]
                 [paint-callback (lambda (c c-dc)
                                   (when refresh-offscreen?
                                     (unless offscreen
                                       (set! offscreen (send c make-bitmap
                                                             (->int (* view-scale total-w))
                                                             (->int (* view-scale total-h))))
                                       (set! offscreen-dc (send offscreen make-dc)))
                                     (set! refresh-offscreen? #f)

                                     (define dc offscreen-dc)
                                     (send dc clear)
                                     (send dc set-smoothing 'smoothed)
                                     
                                     (draw dc))
                                   (send c-dc draw-bitmap offscreen 0 0))]))

  (define (draw dc)
    (draw-graph dc
                #:pkgs pkgs
                #:invert-pkgs invert-pkgs
                #:pkg-bounds pkg-bounds
                #:at-depth at-depth
                #:reps reps
                #:no-build-reps no-build-reps
                #:total-w total-w 
                #:total-h total-h
                #:view-scale view-scale
                #:invert? (send invert-checkbox get-value)
                #:all-deps? (send build-deps-checkbox get-value)
                #:trans-deps? (send trans-deps-checkbox get-value)
                #:hilites hilites))

  (send c init-auto-scrollbars
        (->int (* view-scale total-w))
        (->int (* view-scale total-h))
        0.5 0.5)

  (define hpanel (new horizontal-panel%
                      [parent panel]
                      [alignment '(left center)]
                      [stretchable-height #f]))

  (define build-deps-checkbox
    (new check-box%
         [parent hpanel]
         [label "Show build dependencies"]
         [value all-deps?]
         [callback (lambda (cb e) (send c reset-hilites))]))
  (define trans-deps-checkbox
    (new check-box%
         [parent hpanel]
         [label "Show transitive dependencies"]
         [value trans-deps?]
         [callback (lambda (cb e) (send c reset-hilites))]))
  (define invert-checkbox
    (new check-box%
         [parent hpanel]
         [label "Reverse links"]
         [value invert?]
         [callback (lambda (cb e) (send c reset-hilites))]))

  (send f show #t))
