#lang racket/base
(require "struct.rkt"
         setup/getinfo
         pkg/lib
         setup/dirs
         net/url
         racket/format
         racket/path)

(provide get-pkgs)

(define (get-pkgs #:srcs srcs
                  #:roots root-packages
                  #:quiet quiet?
                  #:no-build? no-build?)

  ;; Get all packages (with possible dangling dependencies)

  (define show-local-catalog-hint? (not quiet?))
  (define show-local-catalog-deps-note? (not quiet?))

  (define (extract-pkg p) (if (string? p) p (car p)))

  (define (add-package-from-dir src-f f-name pkgs)
    (define i (get-info/full src-f))
    (cond
     [i 
      (hash-set pkgs f-name (pkg (map extract-pkg (i 'deps (lambda () null)))
                                 (if no-build?
                                     null
                                     (map extract-pkg (i 'build-deps (lambda () null))))))]
     [else pkgs]))

  (define unfiltered-srcs-pkgs
    (for/fold ([pkgs (hash "racket" (pkg null null))]) ([src (in-list srcs)])
      (case (car src)
        [(dir)
         (define src-dir (cdr src))
         (let loop ([pkgs pkgs] [src-dir src-dir])
           (for/fold ([pkgs pkgs]) ([f (in-list (directory-list src-dir))])
             (define src-f (build-path src-dir f))
             (cond
              [(file-exists? (build-path src-f "info.rkt"))
               (add-package-from-dir src-f (path->string f) pkgs)]
              [(directory-exists? src-f)
               (loop pkgs src-f)]
              [else pkgs])))]
        [(catalog)
         (define (catalog->url s)
           (cond
            [(regexp-match? #rx"^[a-zA-Z]*://" s) (string->url s)]
            [else (path->url (path->complete-path s))]))
         (define catalog (catalog->url (cdr src)))
         (when (and show-local-catalog-hint?
                    (not (equal? "file" (url-scheme catalog))))
           (set! show-local-catalog-hint? #f)
           (printf (~a "Hint: use `raco pkg catalog-copy` to make a local copy of the catalog for\n"
                       " faster viewing\n")))
         (define pkg-details
           (parameterize ([current-pkg-catalogs (list catalog)])
             (get-all-pkg-details-from-catalogs)))
         (for/fold ([pkgs pkgs]) ([(name ht) (in-hash pkg-details)])
           (define deps (hash-ref ht 'dependencies null))
           (when (and show-local-catalog-deps-note?
                      (not (null? deps)))
             (set! show-local-catalog-deps-note? #f)
             (printf (~a "Note: all dependencies from a catalog are treated as run-time dependencies\n")))
           (hash-set pkgs name (pkg (map extract-pkg deps) null)))]
        [else (error "unknown source")])))

  (define unfiltered-pkgs
    (cond
     [(null? srcs)
      (define pkg-scopes
        ;; `pkg/lib` should provide this:
        (append (let ([main (find-pkgs-dir)])
                  (reverse
                   (for/list ([d (get-pkgs-search-dirs)])
                     (if (equal? d main)
                         'installation
                         (simple-form-path d)))))
                '(user)))
      (for/fold ([pkgs unfiltered-srcs-pkgs]) ([pkg-scope (in-list pkg-scopes)])
        (define pkg-names (installed-pkg-names #:scope pkg-scope))
        (parameterize ([current-pkg-scope pkg-scope])
          (for/fold ([pkgs pkgs]) ([pkg (in-list pkg-names)])
            (define dir (pkg-directory pkg))
            (add-package-from-dir dir pkg pkgs))))]
     [else unfiltered-srcs-pkgs]))

  ;; ----------------------------------------

  ;; Filter to roots:
  (define rooted-pkgs
    (cond
     [(null? root-packages) unfiltered-pkgs]
     [else
      (define seen (make-hash))
      (define (loop pkg)
        (cond
         [(hash-ref seen pkg #f) (void)]
         [else
          (define p (hash-ref unfiltered-pkgs pkg #f))
          (when p
            (hash-set! seen pkg #t)
            (for-each loop (pkg-deps p))
            (for-each loop (pkg-build-deps p)))]))
      (for-each loop root-packages)
      (for/hash ([(k v) (in-hash unfiltered-pkgs)]
                 #:when (hash-ref seen k #f))
        (values k v))]))

  ;; Remove dangling dependencies:
  (define pkgs
    (for/hash ([(k v) (in-hash rooted-pkgs)])
      (values k
              (pkg (filter (lambda (x) (hash-ref rooted-pkgs x #f)) 
                           (pkg-deps v))
                   (filter (lambda (x) (hash-ref rooted-pkgs x #f)) 
                           (pkg-build-deps v))))))

  ;; Build inverted tree
  (define invert-pkgs
    (let ([empty-pkg (pkg null null)])
      (define init (for/hash ([k (in-hash-keys pkgs)])
                     (values k empty-pkg)))
      (for/fold ([invert-pkgs init]) ([(k v) (in-hash pkgs)])
        (define ip
          (for/fold ([invert-pkgs invert-pkgs]) ([k2 (in-list (pkg-deps v))])
            (hash-update invert-pkgs
                         k2
                         (lambda (p)
                           (pkg (cons k (pkg-deps p))
                                (pkg-build-deps p))))))
        (for/fold ([invert-pkgs ip]) ([k2 (in-list (pkg-build-deps v))])
          (hash-update invert-pkgs
                       k2
                       (lambda (p)
                         (pkg (pkg-deps p)
                              (cons k (pkg-build-deps p)))))))))

  (values pkgs invert-pkgs))
