#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket's
;assoc library function except (1) it processes a vector (Racket's name for an array) instead of a list and
;(2) it allows vector elements not to be pairs in which case it skips them. Process the vector elements
;in order starting from 0. You must use library functions vector-length, vector-ref, and equal?.
;Return #f if no vector element is a pair with a car eld equal to v, else return the rst pair with an
;equal car eld. Sample solution is 9 lines, using one local recursive helper function.

(define (vector-assoc v vec)
  (letrec  ([len (vector-length vec)]
            [f (lambda(current)
                 (cond[(= current len) #f]
                      [(pair? (vector-ref vec current)) (if (equal? (car (vector-ref vec current)) v)
                                                            (vector-ref vec current)
                                                            (f (+ current 1)))]
                      [#t (f (+ current 1))]))])
    (f 0)))


;Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
;one argument v and returns the same thing that (assoc v xs) would return. However, you should
;use an n-element cache of recent results to possibly make this function faster than just calling assoc (if
;xs is long and a few elements are returned often). The cache must be a Racket vector of length n that
;is created by the call to cached-assoc (use Racket library function vector) and used-and-possibly-
;mutated each time the function returned by cached-assoc is called.

(define (cached-assoc xs n)
  (letrec
      ([cache (make-vector n)]
       [cache-slot 0]
       [find (lambda(x)
               (let ([v-from-cache (vector-assoc x cache)])
                 (if v-from-cache
                     v-from-cache
                     (let ([v-from-xs (vector-assoc x xs)])
                       (if v-from-xs
                           (begin
                             (vector-set! cache cache-slot v-from-xs)
                             (set! cache-slot (remainder (+ cache-slot 1) n))
                             v-from-xs)
                           v-from-xs)))))])
    find))