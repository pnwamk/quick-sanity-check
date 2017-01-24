#lang racket/base

(require racket/set
         rackunit
         rackunit/text-ui
         racket/contract)

(provide
 (contract-out [immutable-equal-set-test-suite
                (->* (string?
                      (-> (and/c generic-set?
                                 (set-implements/c
                                  'set-member?
                                  'set-add
                                  'set-remove
                                  'set-count
                                  'set->stream
                                  'subset?)
                                 set-empty?)))
                     (#:iterations exact-nonnegative-integer?
                      #:operations exact-nonnegative-integer?)
                     test-suite?)]
               [mutable-equal-set-test-suite
                (->* (string?
                      (-> (and/c generic-set?
                                 (set-implements/c
                                  'set-member?
                                  'set-add!
                                  'set-remove!
                                  'set-count
                                  'set->stream
                                  'subset?)
                                 set-empty?)))
                     (#:iterations exact-nonnegative-integer?
                      #:operations exact-nonnegative-integer?)
                     test-suite?)]))


(define (random-string)
  (list->string
   (map integer->char
        (for/list ([i (in-range 1 (add1 (random 20)))])
          (random 256)))))

(define *missing* (gensym 'missing))
(define make-oracle set)
(define make-mutable-oracle mutable-set)
(define num-range 25)

;; - - - - - - - - - - - - - - -
;; assert-suspect/oracle-equal?
;; - - - - - - - - - - - - - - -
(define (assert-suspect/oracle-equal? suspect oracle #:immutable? immutable?)
  (check-equal? (set-empty? suspect)
                (set-empty? oracle))
  (check-equal? (set-count oracle)
                (set-count suspect))
  (check-equal? (set-member? suspect *missing*)
                (set-member? oracle *missing*))
  (subset/set-first suspect oracle immutable?)
  (for ([elem (in-set oracle)])
    (check-true (set-member? suspect elem)))
  (for ([elem (in-set suspect)])
    (check-true (set-member? oracle elem)))
  (set=? (list->set (set->list suspect)) oracle)
  (check-equal? (list->set (set-map suspect (λ (elem) (cons elem elem))))
                (list->set (set-map oracle (λ (elem) (cons elem elem)))))
  (let ([suspect-members (mutable-set)]
        [oracle-members (mutable-set)])
    (set-for-each suspect (λ (elem)  (set-add! suspect-members elem)))
    (set-for-each oracle (λ (elem) (set-add! oracle-members elem)))
    (check-equal? suspect-members oracle-members)))

(define (subset/set-first suspect oracle immutable?)
  (or (and (set-empty? suspect)
           (set-empty? oracle))
      (cond
        [immutable?
         (let loop ([xs suspect])
           (cond
             [(set-empty? xs) #t]
             [else
              (define x   (set-first xs))
              (define rst (set-rest xs))
              (and (set-member? oracle x)
                   (loop rst))]))]
        [else
         (define xs (set-copy suspect))
         (let loop ()
           (cond
             [(set-empty? xs) #t]
             [else
              (define x (set-first xs))
              (set-remove! xs x)
              (and (set-member? oracle x)
                   (loop))]))])))







;; - - - - - - - - - - - - - -
;; check-immutable-equal-set
;; - - - - - - - - - - - - - -
(define (immutable-equal-set-test-suite suite-name
                                        constructor
                                        #:iterations [iters 100]
                                        #:operations [ops   1000])
  (define prechecks (immutable-equal-set-precheck-suite constructor))
  (define randomchecks (immutable-equal-set-random-suite constructor iters ops))
  (test-suite
   suite-name
   prechecks
   randomchecks))

(define (immutable-equal-set-random-suite constructor iters ops)
  (test-suite
   "check-immutable-equal-set random testing"
   (for ([_ (in-range iters)])
     (for/fold ([suspect (constructor)]
                [oracle (make-oracle)])
               ([_ (in-range ops)])
       (assert-suspect/oracle-equal? suspect oracle #:immutable? #f)
       (let ([n (random 100)])
         (cond
           [(<= n 2) (values (set-clear suspect)
                             (set-clear oracle))]
           [(<= n 10) (random-set-remove-half suspect oracle)]
           [(< n 50)  (random-set-add suspect oracle)]
           [(< n 80)  (random-set-remove suspect oracle)]
           [(< n 90)  (random-set-union constructor suspect oracle)]
           [else      (random-set-intersect suspect oracle)]))))))

;; - - - - - - - - - - - - - - - - - - -
;; immutable-equal-set-precheck-suite
;; - - - - - - - - - - - - - - - - - - -
(define (immutable-equal-set-precheck-suite constructor)
  (test-suite
   "check-immutable-equal-set prechecks"
   (let ([s (constructor)]
         [other-s (set-add (set-add (set-add (constructor) 0) 1) 2)])
     (check-true
      (generic-set? s)
      "precheck failed, constructor produced non-generic-set?")
     (check-equal?
      (set-count s) 0
      "precheck failed, constructor produced set with count not equal to 0")
     (check-true
      (set-empty? s)
      "precheck failed, constructor produced non-empty? set")
     (check-false
      (set-member? s 42)
      "precheck failed, constructor produced set which contains element")
     (set! s (set-add s 42))
     (check-false
      (set-empty? s)
      "precheck failed, set-add resulted in an empty set")
     (check-equal?
      (set-count s) 1
      "precheck failed, set-add did not add an element (or set-count does not account for it)")
     (check-true
      (set-member? s 42)
      "precheck failed, set-add did not add an element (or set-member? cannot find it)")
     (set! s (set-remove s 42))
     (check-equal?
      (set-count s) 0
      "precheck failed, set-remove did not remove an element (or set-count is off)")
     (check-false
      (set-member? s 42)
      "precheck failed, set-remove did not remove an element")
     (set! s (set-add s 2))
     (set! s (set-add s 3))
     (set! s (set-add s 4))
     (set! s (set-union s))
     (check-equal?
      (set-count s) 3
      "precheck failed, set-union with a single argument modified the set")
     (set! s (set-intersect s))
     (check-equal?
      (set-count s) 3
      "precheck failed, set-intersect with a single argument modified the set")
     (set! s (set-intersect s other-s))
     (check-equal?
      (set-count s) 1
      "precheck failed, set-intersect failed")
     (check-true
      (set-member? s 2)
      "precheck failed, set-intersect did not leave the correct element")
     (set! s (set-union s other-s))
     (check-equal?
      (set-count s) 3
      "precheck failed, set-remove did not remove an element (or set-count is off)")
     (check-true
      (and (set-member? s 0)
           (set-member? s 1)
           (set-member? s 2))
      "precheck failed, set-union did not add all the expected elements"))))

;; - - - - - - - - -
;; random-set-add
;; - - - - - - - - -
(define (random-set-add suspect oracle)
  (define elem
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (random num-range)
        (random-string)))
  (values (set-add suspect elem)
          (set-add oracle elem)))

;; - - - - - - - - - -
;; random-set-remove
;; - - - - - - - - - -
(define (random-set-remove suspect oracle)
  (define key
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (random num-range)
        (random-string)))
  (values (set-remove suspect key)
          (set-remove oracle key)))

;; - - - - - - - - - - - - -
;; random-set-remove-half
;; - - - - - - - - - - - - -
(define (random-set-remove-half suspect oracle)
  (for/fold ([suspect suspect]
             [oracle oracle])
            ([key (in-set oracle)]
             [n (in-naturals)]
             #:when (even? n))
    (values (set-remove suspect key)
            (set-remove oracle key))))


;; - - - - - - - - -
;; random-set-union
;; - - - - - - - - -
(define (random-set-union constructor suspect oracle)
  (define-values (s1 s2 o1 o2)
    (for/fold ([s1 (constructor)]
               [s2 (constructor)]
               [o1 (make-oracle)]
               [o2 (make-oracle)])
              ([new-elem (build-list 15 (λ _ random-string))]
               [n (in-naturals)])
      (cond
        [(even? n)
         (values (set-add s1 new-elem)
                 s2
                 (set-add o1 new-elem)
                 o2)]
        [else
         (values s1
                 (set-add s2 new-elem)
                 o1
                 (set-add o2 new-elem))])))
  (values (set-union suspect s1 s2)
          (set-union oracle o1 o2)))


;; - - - - - - - - - - -
;; random-set-intersect
;; - - - - - - - - - - -
(define (random-set-intersect suspect oracle)
  (define-values (1/2suspect 1/2oracle)
    (random-set-remove-half suspect oracle))
  (define-values (1/4suspect 1/4oracle)
    (random-set-remove-half 1/2suspect 1/2oracle))
  (values (set-intersect suspect 1/2suspect 1/4suspect)
          (set-intersect oracle  1/2oracle  1/4oracle)))












;; - - - - - - - - - - - - - - -
;; mutable-equal-set-test-suite
;; - - - - - - - - - - - - - - -
(define (mutable-equal-set-test-suite suite-name
                                      constructor
                                      #:iterations [iters 100]
                                      #:operations [ops   1000])
  (define prechecks (mutable-equal-set-precheck-suite constructor))
  (define randomchecks (mutable-equal-set-random-suite constructor iters ops))
  (test-suite
   suite-name
   prechecks
   randomchecks))


;; - - - - - - - - - - - - - - - -
;; mutable-equal-set-random-suite
;; - - - - - - - - - - - - - - - -
(define (mutable-equal-set-random-suite constructor iters ops)
  (test-suite
   "check-mutable-equal-set random testing"
   (for ([_ (in-range iters)])
     (define suspect (constructor))
     (define oracle (make-mutable-oracle))
     (assert-suspect/oracle-equal? suspect oracle #:immutable? #f)
     (for ([_ (in-range ops)])
       (let ([n (random 100)])
         (cond
           [(<= n 2) (set-clear! suspect)
                     (set-clear! oracle)]
           [(<= n 10) (random-set-remove-half! suspect oracle)]
           [(< n 50)  (random-set-add! suspect oracle)]
           [(< n 80)  (random-set-remove! suspect oracle)]
           [(< n 90)  (random-set-union! constructor suspect oracle)]
           [else      (random-set-intersect! constructor suspect oracle)]))
       (assert-suspect/oracle-equal? suspect oracle #:immutable? #f)))))


;; - - - - - - - - - - - - - - -
;; precheck-mutable-equal-set
;; - - - - - - - - - - - - - - -
(define (mutable-equal-set-precheck-suite constructor)
  (test-suite
   "check-immutable-equal-set prechecks"
   (let ([s (constructor)]
         [other-s (constructor)])
     (set-add! other-s 0)
     (set-add! other-s 1)
     (set-add! other-s 2)
     (check-true
      (generic-set? s)
      "precheck failed, constructor produced non-generic-set?")
     (check-equal?
      (set-count s) 0
      "precheck failed, constructor produced set with count not equal to 0")
     (check-true
      (set-empty? s)
      "precheck failed, constructor produced non-empty? set")
     (check-false
      (set-member? s 42)
      "precheck failed, constructor produced set which contains element")
     (set-add! s 42)
     (check-false
      (set-empty? s)
      "precheck failed, set-add resulted in an empty set")
     (check-equal?
      (set-count s) 1
      "precheck failed, set-add did not add an element (or set-count does not account for it)")
     (check-true
      (set-member? s 42)
      "precheck failed, set-add did not add an element (or set-member? cannot find it)")
     (set-remove! s 42)
     (check-equal?
      (set-count s) 0
      "precheck failed, set-remove did not remove an element (or set-count is off)")
     (check-false
      (set-member? s 42)
      "precheck failed, set-remove did not remove an element")
     (set-add! s 2)
     (set-add! s 3)
     (set-add! s 4)
     (set-union! s)
     (check-equal?
      (set-count s) 3
      "precheck failed, set-union with a single argument modified the set")
     (set-intersect! s)
     (check-equal?
      (set-count s) 3
      "precheck failed, set-intersect with a single argument modified the set")
     (set-intersect! s other-s)
     (check-equal?
      (set-count s) 1
      "precheck failed, set-intersect failed")
     (check-true
      (set-member? s 2)
      "precheck failed, set-intersect did not leave the correct element")
     (set-union! s other-s)
     (check-equal?
      (set-count s) 3
      "precheck failed, set-remove did not remove an element (or set-count is off)")
     (check-true
      (and (set-member? s 0)
           (set-member? s 1)
           (set-member? s 2))
      "precheck failed, set-union did not add all the expected elements"))))




;; - - - - - - - - -
;; random-set-add!
;; - - - - - - - - -
(define (random-set-add! suspect oracle)
  (define elem
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (random num-range)
        (random-string)))
  (set-add! suspect elem)
  (set-add! oracle elem))

;; - - - - - - - - - -
;; random-set-remove!
;; - - - - - - - - - -
(define (random-set-remove! suspect oracle)
  (define elem
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (random num-range)
        (random-string)))
  (set-remove! suspect elem)
  (set-remove! oracle  elem))

;; - - - - - - - - - - - - -
;; random-set-remove-half!
;; - - - - - - - - - - - - -
(define (random-set-remove-half! suspect oracle)
  (for ([elem (in-list (set->list oracle))]
        [n (in-naturals)]
        #:when (even? n))
    (set-remove! suspect elem)
    (set-remove! oracle  elem)))


;; - - - - - - - - - -
;; random-set-union!
;; - - - - - - - - - -
(define (random-set-union! constructor suspect oracle)
  (define-values (s1 s2 o1 o2)
    (let ([s1 (constructor)]
          [s2 (constructor)]
          [o1 (make-mutable-oracle)]
          [o2 (make-mutable-oracle)])
      (for ([new-elem (build-list 15 (λ _ random-string))]
            [n (in-naturals)])
        (cond
          [(odd? n)
           (set-add! s1 new-elem)
           (set-add! o1 new-elem)]
          [else
           (set-add! s2 new-elem)
           (set-add! o2 new-elem)]))
      (values s1 s2 o1 o2)))
  (set-union! suspect s1 s2)
  (set-union! oracle o1 o2))


;; - - - - - - - - - - -
;; random-set-intersect!
;; - - - - - - - - - - -
(define (random-set-intersect! constructor suspect oracle)
  (define-values (s1 s2 o1 o2)
    (let ([s1 (constructor)]
          [s2 (constructor)]
          [o1 (make-mutable-oracle)]
          [o2 (make-mutable-oracle)])
      (for ([elem (in-list (set->list oracle))]
            [n (in-naturals)]
            #:when (even? n))
        (set-add! s1 elem)
        (set-add! s1 (random 100000))
        (set-add! s2 elem)
        (set-add! s2 (random 100000))
        (set-add! o1 elem)
        (set-add! o1 (random 100000))
        (set-add! o2 elem)
        (set-add! o2 (random 100000)))
      (values s1 s2 o1 o2)))
  (values (set-intersect! suspect s1 s2)
          (set-intersect! oracle o1 o2)))
