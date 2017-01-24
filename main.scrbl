#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label racket/base
                     racket/contract
                     rackunit
                     "main.rkt"))

@title{Quick Sanity Check: Generic Interface Testing}
@author{@(author+email "Andrew Kent" "andmkent@iu.edu")}


@defmodule[quick-sanity-check]

This package defines functions which build "sanity check"
black-box test suites for a struct's implementation of the
the @racket[gen:set] (i.e.
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 generic-set}) or @racket[gen:dict] (i.e.
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 dictionary}) interface (mutable or immutable).

Basically, the constructed test suites first run a few basic
tests to see if something is obviously broken. Then checking
proceeds by performing a large number of random operations
on the given set. Identical random operations are performed
on a reference implementation and the dicts/sets are
compared to ensure they are in sync (this is all done using
the various dict-* or set-* functions which allow for the
inspection of the elements, cardinality, etc).

Currently these tests do not claim to be all encompassing or
perfect (your contributions & improvements are welcome!) but
they cover a good chunk of the interface and are a nice
cheap addition to your own tests.

Also, currently this package is geared towards checking
dicts and sets which reason about @racket[equal?]-based
equality only. In the future more options may be added, but
rest assured the current functions will remain (that's why
they have been given such specific names---if a more
flexible interface is introduced these then ``legacy''
procedures will still work just fine).


@section{Dict Testing}

@defproc[(immutable-equal-dict-test-suite
          [suite-name string?]
          [constructor
           (-> (and/c dict?
                      (dict-implements/c
                       'dict-ref
                       'dict-set
                       'dict-remove
                       'dict-count
                       'dict-iterate-first
                       'dict-iterate-next
                       'dict-iterate-key
                       'dict-iterate-value)
                      dict-empty?))]
          [#:iterations iters exact-nonnegative-integer? 100]
          [#:operations ops exact-nonnegative-integer? 1000])
         test-suite?]{

 Builds a rackunit test suite named @racket[suite-name] which, when run,
 will perform a series of tests on the empty immutable dicts
 produced by @racket[constructor] using only operations from the
 @racket[gen:dict] interface. The random portion of the
 testing will perform @racket[ops] random operations on an
 initially empty dict @racket[iters] times.

 In other words, something with @italic{roughly} the
 following shape is returned:

 @racketblock[(test-suite
               suite-name
               (test-suite
                "prechecks"
                (check-equal? (dict-ref (dict-set (constructor) 1 "1") 1) "1")
                ...)
               (test-suite
                "random checks"
                (for ([_ (in-range iters)])
                  (for/fold ([d (constructor)])
                            ([_ (in-range ops)])
                    (perform-a-random-test d)))))]
}


@defproc[(mutable-equal-dict-test-suite
          [suite-name string?]
          [constructor
           (-> (and/c dict?
                      (dict-implements/c
                       'dict-ref
                       'dict-set!
                       'dict-remove!
                       'dict-count
                       'dict-iterate-first
                       'dict-iterate-next
                       'dict-iterate-key
                       'dict-iterate-value)
                      dict-empty?))]
          [#:iterations iters exact-nonnegative-integer? 100]
          [#:operations ops exact-nonnegative-integer? 1000])
         test-suite?]{

 Builds a rackunit test suite named @racket[suite-name] which, when run,
 will perform a series of tests on the empty mutable dicts
 produced by @racket[constructor] using only operations from the
 @racket[gen:dict] interface. The random portion of the
 testing will perform @racket[ops] random operations on an
 initially empty dict @racket[iters] times.

 In other words, something with @italic{roughly} the
 following shape is returned:

 @racketblock[(test-suite
               suite-name
               (test-suite
                "prechecks"
                (check-equal? (let ([d (constructor)])
                                (dict-set! d 1 "1")
                                (dict-ref d 1))
                              "1")
                ...)
               (test-suite
                "random checks"
                (for ([_ (in-range iters)])
                  (define d (constructor))
                  (for ([_ (in-range ops)])
                    (perform-a-random-test! d)))))]}

@section{Set Testing}

@defproc[(immutable-equal-set-test-suite
          [suite-name string?]
          [constructor
           (-> (and/c generic-set?
                      (set-implements/c
                       'set-member?
                       'set-add
                       'set-remove
                       'set-count
                       'set->stream
                       'subset?)
                      set-empty?))]
          [#:iterations iters exact-nonnegative-integer? 100]
          [#:operations ops exact-nonnegative-integer? 1000])
         test-suite?]{

 Builds a rackunit test suite named @racket[suite-name] which, when run,
 will perform a series of tests on the empty immutable sets
 produced by @racket[constructor] using only operations from the
 @racket[gen:set] interface. The random portion of the
 testing will perform @racket[ops] random operations on an
 initially empty set @racket[iters] times.

 In other words, something with @italic{roughly} the
 following shape is returned:

 @racketblock[(test-suite
               suite-name
               (test-suite
                "prechecks"
                (check-true (set-member? (set-add (constructor) 1) 1))
                ...)
               (test-suite
                "random checks"
                (for ([_ (in-range iters)])
                  (for/fold ([s (constructor)])
                            ([_ (in-range ops)])
                    (perform-a-random-test s)))))]
}


@defproc[(mutable-equal-set-test-suite
          [suite-name string?]
          [constructor
           (-> (and/c generic-set?
                      (set-implements/c
                       'set-member?
                       'set-add!
                       'set-remove!
                       'set-count
                       'set->stream
                       'subset?)
                      set-empty?))]
          [#:iterations iters exact-nonnegative-integer? 100]
          [#:operations ops exact-nonnegative-integer? 1000])
         test-suite?]{

 Builds a rackunit test suite named @racket[suite-name] which, when run,
 will perform a series of tests on the empty mutable sets
 produced by @racket[constructor] using only operations from the
 @racket[gen:set] interface. The random portion of the
 testing will perform @racket[ops] random operations on an
 initially empty set @racket[iters] times.

 In other words, something with @italic{roughly} the
 following shape is returned:

 @racketblock[(test-suite
               suite-name
               (test-suite
                "prechecks"
                (check-true (let ([s (constructor)])
                              (set-add! s 1)
                              (set-member? s 1)))
                ...)
               (test-suite
                "random checks"
                (for ([_ (in-range iters)])
                  (define s (constructor))
                  (for ([_ (in-range ops)])
                    (perform-a-random-test! s)))))]}
