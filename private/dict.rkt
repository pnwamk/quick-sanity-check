#lang racket/base

(require racket/dict
         racket/set
         rackunit
         racket/contract)

(provide
 (contract-out [immutable-equal-dict-test-suite
                (->* (string?
                      (-> (and/c dict?
                                 (dict-implements/c
                                  'dict-ref
                                  'dict-set
                                  'dict-remove
                                  'dict-iterate-first
                                  'dict-iterate-next
                                  'dict-iterate-key
                                  'dict-iterate-value)
                                 dict-empty?)))
                     (#:iterations exact-nonnegative-integer?
                      #:operations exact-nonnegative-integer?)
                     test-suite?)]
               [mutable-equal-dict-test-suite
                (->* (string?
                      (-> (and/c dict?
                                 (dict-implements/c
                                  'dict-ref
                                  'dict-set!
                                  'dict-remove!
                                  'dict-iterate-first
                                  'dict-iterate-next
                                  'dict-iterate-key
                                  'dict-iterate-value)
                                 dict-empty?)))
                     (#:iterations exact-nonnegative-integer?
                      #:operations exact-nonnegative-integer?)
                     test-suite?)]))


(define (random-string)
  (list->string
   (map integer->char
        (for/list ([i (in-range 1 (add1 (random 20)))])
          (random 256)))))

(define *missing* (gensym 'missing))
(define make-oracle hash)
(define make-mutable-oracle make-hash)

;; - - - - - - - - - - - - - - -
;; assert-suspect/oracle-equal?
;; - - - - - - - - - - - - - - -
(define (assert-suspect/oracle-equal? suspect oracle)
  (check-equal? (dict-empty? suspect)
                (dict-empty? oracle))
  (check-equal? (dict-count oracle)
                (dict-count suspect))
  (check-equal? (dict-has-key? suspect *missing*)
                (dict-has-key? oracle *missing*))
  (check-exn exn:fail? (λ () (dict-ref suspect *missing*)))
  (check-exn exn:fail? (λ () (dict-ref oracle *missing*)))
  (check-equal? (dict-ref suspect *missing* *missing*)
                (dict-ref oracle *missing* *missing*))
  (for ([(key val) (in-dict oracle)])
    (check-true (dict-has-key? suspect key))
    (check-equal? (dict-ref suspect key) val)
    (check-equal? (dict-ref suspect key *missing*) val))
  (for ([(key val) (in-dict suspect)])
    (check-true (dict-has-key? oracle key))
    (check-equal? (dict-ref oracle key) val)
    (check-equal? (dict-ref oracle key *missing*) val))
  (check-equal? (list->set (dict->list suspect))
                (list->set (dict->list oracle)))
  (check-equal? (list->set (dict-keys suspect))
                (list->set (dict-keys oracle)))
  (check-equal? (list->set (dict-values suspect))
                (list->set (dict-values oracle)))
  (check-equal? (list->set (dict-map suspect (λ (k v) (cons k v))))
                (list->set (dict-map oracle (λ (k v) (cons k v)))))
  (let ([suspect-members (mutable-set)]
        [oracle-members (mutable-set)])
    (dict-for-each suspect (λ (k v) (set-add! suspect-members (cons k v))))
    (dict-for-each oracle (λ (k v) (set-add! oracle-members (cons k v))))
    (check-equal? suspect-members
                  oracle-members))
  (check-equal? (for/set ([key (in-dict-keys suspect)]) key)
                (for/set ([key (in-dict-keys oracle)]) key))
  (check-equal? (for/set ([val (in-dict-values suspect)]) val)
                (for/set ([val (in-dict-values oracle)]) val)))







;; - - - - - - - - - - - - - -
;; check-immutable-equal-dict
;; - - - - - - - - - - - - - -
(define (immutable-equal-dict-test-suite suite-name
                                         constructor
                                         #:iterations [iters 100]
                                         #:operations [ops   1000])
  (define prechecks (immutable-equal-dict-precheck-suite constructor))
  (define randomchecks (immutable-equal-dict-random-suite constructor iters ops))
  (test-suite
   suite-name
   prechecks
   randomchecks))

(define (immutable-equal-dict-random-suite constructor iters ops)
  (test-suite
   "check-immutable-equal-dict random testing"
   (for ([_ (in-range iters)])
     (for/fold ([suspect (constructor)]
                [oracle (make-oracle)])
               ([_ (in-range ops)])
       (assert-suspect/oracle-equal? suspect oracle)
       (let ([n (random 100)])
         (cond
           [(<= n 2) (values (dict-clear suspect)
                             (dict-clear oracle))]
           [(<= n 10) (random-dict-remove-half suspect oracle)]
           [(< n 40) (random-dict-set suspect oracle)]
           [(< n 80) (random-dict-remove suspect oracle)]
           [else     (random-dict-update suspect oracle)]))))))

;; - - - - - - - - - - - - - - - - - - -
;; precheck-immutable-equal-dict-suite
;; - - - - - - - - - - - - - - - - - - -
(define (immutable-equal-dict-precheck-suite constructor)
  (test-suite
   "check-immutable-equal-dict prechecks"
   (let ([d (constructor)])
     (check-true
      (dict? d)
      "precheck failed, constructor produced non-dict?")
     (check-equal?
      (dict-count d) 0
      "precheck failed, constructor produced dict with count not equal to 0")
     (check-true
      (dict-empty? d)
      "precheck failed, constructor produced non-empty? dict")
     (check-false
      (dict-ref d 42 #f)
      "precheck failed, constructor produced dict which contains element")
     (check-equal?
      (dict-ref d 42 "Hello")
      "Hello"
      "precheck failed, dict-ref returned wrong default value")
     (check-equal?
      (dict-ref d 42 (λ () "World!"))
      "World!"
      "precheck failed, dict-ref returned wrong default value")
     (set! d (dict-set d 42 41))
     (check-equal?
      (dict-count d) 1
      "precheck failed, dict-set did not add an element (or dict-count does not account for it)")
     (check-equal?
      (dict-ref d 42 #f)
      41
      "precheck failed, dict-set did not add an element (or dict-ref cannot find it)")
     (set! d (dict-update d 42 add1))
     (check-equal?
      (dict-count d) 1
      "precheck failed, dict-count changed by update to present element")
     (check-equal?
      (dict-ref d 42 #f)
      42
      "precheck failed, dict-update did not modify an element (or dict-ref cannot find it)")
     (set! d (dict-remove d 42))
     (check-equal?
      (dict-count d) 0
      "precheck failed, dict-remove did not remove an element (or dict-count is off)")
     (check-false
      (dict-ref d 42 #f)
      "precheck failed, dict-remove did not remove an element")
     (set! d (dict-update d 42 add1 41))
     (check-equal?
      (dict-count d) 1
      "precheck failed, dict-update did not add and update an element (or dict-count is off)")
     (check-equal?
      (dict-ref d 42 #f)
      42
      "precheck failed, dict-update did not add an element (or dict-ref cannot find it)")
     (set! d (dict-remove d 42))
     (set! d (dict-update d 40 add1 (λ () 39)))
     (check-equal?
      (dict-ref d 40 #f)
      40
      "precheck failed, dict-update did not use the right default value")
     (set! d (dict-remove d 40))
     (set! d (dict-set* d))
     (set! d (dict-set* d 1 2 3 4))
     (check-equal?
      (dict-count d) 2
      "precheck failed, dict-set* did not add the given elements")
     (check-equal?
      (dict-ref d 1)
      2
      "precheck failed, dict-set* did not add the first element")
     (check-equal?
      (dict-ref d 3)
      4
      "precheck failed, dict-set* did not add the second element"))))

;; - - - - - - - - -
;; random-dict-set
;; - - - - - - - - -
(define (random-dict-set suspect oracle)
  (define-values (key val)
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (values (random 50) (random 50))
        (values (random-string) (random-string))))
  (values (dict-set suspect key val)
          (dict-set oracle key val)))

;; - - - - - - - - - -
;; random-dict-remove
;; - - - - - - - - - -
(define (random-dict-remove suspect oracle)
  (define key
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (random 50)
        (random-string)))
  (values (dict-remove suspect key)
          (dict-remove oracle key)))

;; - - - - - - - - - - - - -
;; random-dict-remove-half
;; - - - - - - - - - - - - -
(define (random-dict-remove-half suspect oracle)
  (for/fold ([suspect suspect]
             [oracle oracle])
            ([key (in-dict-keys oracle)]
             [n (in-naturals)]
             #:when (even? n))
    (values (dict-remove suspect key)
            (dict-remove oracle key))))

;; - - - - - - - - - -
;; random-dict-update
;; - - - - - - - - - -
(define (random-dict-update suspect oracle)
  (define key (random 50))
  (define not-found-val (random 50))
  (define failure (if (even? not-found-val)
                      not-found-val
                      (λ () not-found-val)))
  
  (values (dict-update suspect key add1 failure)
          (dict-update oracle key add1 failure)))






;; - - - - - - - - - - - - -
;; check-mutable-equal-dict
;; - - - - - - - - - - - - -

(define (mutable-equal-dict-test-suite suite-name
                                       constructor
                                       #:iterations [iters 100]
                                       #:operations [ops   1000])
  (define prechecks (mutable-equal-dict-precheck-suite constructor))
  (define randomchecks (mutable-equal-dict-random-suite constructor iters ops))
  (test-suite
   suite-name
   prechecks
   randomchecks))


(define (mutable-equal-dict-random-suite constructor iters ops)
  (test-suite
   "check-mutable-equal-dict random testing"
   (for ([_ (in-range iters)])
     (define suspect (constructor))
     (define oracle (make-mutable-oracle))
     (assert-suspect/oracle-equal? suspect oracle)
     (for ([_ (in-range ops)])
       (let ([n (random 100)])
         (cond
           [(<= n 2) (dict-clear! suspect)
                     (dict-clear! oracle)]
           [(<= n 10) (random-dict-remove-half! suspect oracle)]
           [(< n 40) (random-dict-set! suspect oracle)]
           [(< n 80) (random-dict-remove! suspect oracle)]
           [(< n 90) (random-dict-update! suspect oracle)]
           [else     (random-dict-ref! suspect oracle)]))
       (assert-suspect/oracle-equal? suspect oracle)))))



;; - - - - - - - - - - - - - - -
;; precheck-immutable-equal-dict
;; - - - - - - - - - - - - - - -
(define (mutable-equal-dict-precheck-suite constructor)
  (test-suite
   "check-mutable-equal-dict prechecks"
   (let ([d (constructor)])
     (check-true
      (dict? d)
      "precheck failed, constructor produced non-dict?")
     (check-equal?
      (dict-count d) 0
      "precheck failed, constructor produced dict with count not equal to 0")
     (check-true
      (dict-empty? d)
      "precheck failed, constructor produced non-empty? dict")
     (check-false
      (dict-ref d 42 #f)
      "precheck failed, constructor produced dict which contains element")
     (check-equal?
      (dict-ref d 42 "Hello")
      "Hello"
      "precheck failed, dict-ref returned wrong default value")
     (check-equal?
      (dict-ref d 42 (λ () "World!"))
      "World!"
      "precheck failed, dict-ref returned wrong default value")
     (dict-set! d 42 41)
     (check-equal?
      (dict-count d) 1
      "precheck failed, dict-set! did not add an element (or dict-count does not account for it)")
     (check-equal?
      (dict-ref d 42 #f)
      41
      "precheck failed, dict-set! did not add an element (or dict-ref cannot find it)")
     (dict-update! d 42 add1)
     (check-equal?
      (dict-count d) 1
      "precheck failed, dict-count changed by update to present element")
     (check-equal?
      (dict-ref d 42 #f)
      42
      "precheck failed, dict-update! did not modify an element (or dict-ref cannot find it)")
     (dict-remove! d 42)
     (check-equal?
      (dict-count d) 0
      "precheck failed, dict-remove! did not remove an element (or dict-count is off)")
     (check-false
      (dict-ref d 42 #f)
      "precheck failed, dict-remove! did not remove an element")
     (dict-update! d 42 add1 41)
     (check-equal?
      (dict-count d) 1
      "precheck failed, dict-update! did not add and update an element (or dict-count is off)")
     (check-equal?
      (dict-ref d 42 #f)
      42
      "precheck failed, dict-update! did not add an element (or dict-ref cannot find it)")
     (dict-remove! d 42)
     (dict-update! d 40 add1 (λ () 39))
     (check-equal?
      (dict-ref d 40 #f)
      40
      "precheck failed, dict-update! did not use the right default value")
     (dict-remove! d 40)
     (dict-set*! d)
     (dict-set*! d 1 2 3 4)
     (check-equal?
      (dict-count d) 2
      "precheck failed, dict-set* did not add the given elements")
     (check-equal?
      (dict-ref d 1)
      2
      "precheck failed, dict-set* did not add the first element")
     (check-equal?
      (dict-ref d 3)
      4
      "precheck failed, dict-set* did not add the second element"))))




;; - - - - - - - - -
;; random-dict-set!
;; - - - - - - - - -
(define (random-dict-set! suspect oracle)
  (define-values (key val)
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (values (random 50) (random 50))
        (values (random-string) (random-string))))
  (dict-set! suspect key val)
  (dict-set! oracle key val))

;; - - - - - - - - - -
;; random-dict-remove!
;; - - - - - - - - - -
(define (random-dict-remove! suspect oracle)
  (define key
    ;; ensure we're getting some key collisions
    (if (even? (random 3))
        (random 50)
        (random-string)))
  (dict-remove! suspect key)
  (dict-remove! oracle  key))

;; - - - - - - - - - - - - -
;; random-dict-remove-half
;; - - - - - - - - - - - - -
(define (random-dict-remove-half! suspect oracle)
  (for ([key (in-list (dict-keys oracle))]
        [n (in-naturals)]
        #:when (even? n))
    (dict-remove! suspect key)
    (dict-remove! oracle key)))

;; - - - - - - - - - -
;; random-dict-update!
;; - - - - - - - - - -
(define (random-dict-update! suspect oracle)
  (define key (random 50))
  (define not-found-val (random 50))
  (define failure (if (even? not-found-val)
                      not-found-val
                      (λ () not-found-val)))
  
  (dict-update! suspect key add1 failure)
  (dict-update! oracle key add1 failure))

;; - - - - - - - - - -
;; random-dict-ref!
;; - - - - - - - - - -
(define (random-dict-ref! suspect oracle)
  (define key (random 50))
  (define not-found-val (random 50))
  (define failure (if (even? not-found-val)
                      not-found-val
                      (λ () not-found-val)))
  
  (dict-ref! suspect key failure)
  (dict-ref! oracle key failure))

