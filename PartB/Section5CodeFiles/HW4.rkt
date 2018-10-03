
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

#| this makes sure to use TCO for compute optimization
as opposed to a shorter function which doesn't use TCO |#

(define (sequence low high stride)
  (letrec ([helper (lambda (low2 acc) ; create a helper function with an accumulator, which will hold be the returned value
                     (if (> low2 high) ; if the low value is greater than the high value
                         (reverse acc) ; return the acc (reversed b/c of cons rules for null)
                         (helper (+ low2 stride) (cons low2 acc))))]) ; else recurse the helper with updated low and acc values
    (helper low null)))

(define (string-append-map xs suffix)
  (map (lambda (hdxs) (string-append hdxs suffix)) xs)) ; put the "suffix" onto an element in "xs" using "string-append" and then do this for all elements in "xs" using map

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")] ; error if n < 0
        [(null? xs) (error "list-nth-mod: empty list")] ; error of xs is empty
        [#t ; else
         (let ([i (remainder n (length xs))]) ; let i = n mod length xs...
           (car (list-tail xs i)))])) ; and return that element i from the "list-tail" since we are counting from 0

(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (s2 n2 acc) ; create a helper function with an accumulator and a deprecating counter "n2"
                     (cond [(= n2 0) (reverse acc)] ; if counter reaches 0 return reversed acc
                           [#t (helper (cdr (s2)) (- n2 1) (cons (car (s2)) acc))]))]) ; else recurse the helper with the tail of the stream and cons the head of the stream to acc
    (helper s n null)))

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream ; same as nats but replace every 5th element by it's negative
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (* -1 x) (lambda () (f (+ x 1))))] ; if it's a 5th element, cons a negative value onto the thunk (stream)
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cond [( = (remainder x 2) 0) (cons "dog.jpg" (lambda () (f (+ x 1))))] ; case for consing "dog.jpg" onto thunk (stream)
                      [#t (cons "dan.jpg" (lambda () (f (+ x 1))))]))]) ; else cons "dan.jpg" onto thunk (stream)
    (lambda () (f 1)))) ; make sure we start with "dan.jpg"

(define (stream-add-zero s) 
  (letrec ([f (lambda (x) 
                (lambda () (cons (cons 0 (car (x))) (f (cdr (x))))))]) ;cons 0 to the head of the unkthunked stream ("x"), and cons that result onto the cdr, and thunk the whole thing
    (f s))) ; make sure x in the anonymous function above is the evaluation of the stream itself

(define (cycle-lists xs ys) 
  (letrec ([helper (lambda (n) ; create a helper function that will take in the element we need "n" of both lists
                     (lambda () (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) ; cons the "nth" values of both lists together to create a pair
                                      (helper (+ n 1)))))]) ; and cons that result onto the helper function itself, and thunk the whole thing, creating a stream
    (helper 0))) ; start from element 0

(define (vector-assoc v vec)
  (letrec  ([vL (vector-length vec)] ;create variable "len" for overall length of the vector "vec"
            [helper (lambda(cur) ; create helper function
                      (cond[(= cur vL) #f] ; if we have reached the end of the vector "vec" without a match, return false
                           [(pair? (vector-ref vec cur)) ; else if the current element "cur" in the vector "vec" is a pair...
                            (cond [(equal? (car (vector-ref vec cur)) v) ; & if the head of "cur" is equal to v
                                   (vector-ref vec cur)] ; return "cur"
                                  [#t (helper (+ cur 1))])] ; else recurse the helper function with the next vector element
                           [#t (helper (+ cur 1))]))]) ; else if the current vector element is not a pair, recurse helper 
    (helper 0)))

(define (cached-assoc xs n)
  (letrec
      ([memo (make-vector n #f)] ; create memo
       [memoSlot 0] ; initial slot in memo
       [f (lambda(v)
            (let* ([vecMemo (vector-assoc v memo)]) ; check memo to see if answer has already been computed
              (if vecMemo  ; if answer has been previously computed...
                  vecMemo ; return that same answer
                  (let* ([vPair (assoc v xs)]) ; else if answer is not found in memo...
                    (if vPair ; and if there is an answer to "(assoc v xs)"...
                        (begin (vector-set! memo memoSlot vPair) ; add it to memo
                               (set! memoSlot (if (= (+ memoSlot 1) n) 0 (+ memoSlot 1))) vPair) ; return vPair
                        #f)))))]) ; else, return false
    f)) ; return function "f"

(define-syntax while-less
  (syntax-rules (do)
    ((while-less e1 do e2)
     (let ([e1Cp e1])
       (letrec ([f (lambda ()
                        (let ([e2Cp e2])
                          (if (>= e2Cp e1Cp)
                              #t
                              (f))))])
         (f))))))