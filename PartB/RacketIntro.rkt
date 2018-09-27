#lang racket

(provide (all-defined-out))

(define x 3)
(define y (+ x 2))

(define cube1
  (lambda (x)
    (* x (* x x))))

(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x) ; syntacic sugar for anonymous functions (replacing lambda)
  (* x x x))

(define (pow1 x y) ; example of using conditionals (if e1 e2 e3)
  (if (= y 0) 1 (* x (pow1 x (- y 1)))))

; currying example, but not as common in racket b/c of racket's built-in
; support for multi-arg functions

(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

 (define threeToTheY (pow2 3))

; syntactic sugar for currying

(define ((pow2b x) y) (pow1 x y))

(define (sum xs)
  (if (null? xs) 0 (+ (first xs) (sum (rest xs)))))

(define (myAppend xs ys)
  (if (null? xs) ys (cons (first xs) (myAppend (rest xs) ys))))

(define (myMap f xs)
  (if (null? xs) null (cons (f (first xs) (myMap f (rest xs))))))

;s-expression comment ("#;")

(define (myAppendAcc xs ys)
  (let helper ([xs2 xs] [ys2 ys] [acc null])
         (if (null? xs2)
             (cons acc ys2)
             (helper (rest xs2) ys2 (cons acc (first xs2))))))

#| multi-line
comment |#

#| interesting let-expression case: (note we don't have to
 close each let-expression individually - we can close all together
(let ([ x 5]) (let ([x 2] [y x]) (let ([x 3] [z x]) (list x y z))))
 {can only use bindings before let-expression}
(let* ([x 5] [y x] [x 2] [z x] [x 3])) (list x y z)
 {can use bindings before and before-within let expression}
(letrec...) {can use bindings before, before-within, and after-within
 let expression - useful for mutual recursion |#

#| advantage of dynamic typing example: summing up a list that contains
numbers or nested lists (which in turn can have nested lists, etc...)
without having to create a datatype binding with constructors which allows
for this feature of referring to nested lists. Don't need a datatype binding
here because we don't type-check our example before it runs, which allows us
to mix together types more easily |#

; example lists:

(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (first xs))
          (+ (first xs) (sum1 (rest xs)))
          (if (list? (first xs))
              (+ (sum1 (first xs)) (sum1 (rest xs)))
              (+ (sum1 (rest xs)))))))

; sum with cond
(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (first xs)) (+ (first xs) (sum1 (rest xs)))]
        [(list? (first xs)) (+ (sum1 (first xs)) (sum1 (rest xs)))]
        [#t (+ (sum1 (rest xs)))]
        ))

#| semantics of if/cond: "treat anything other than #f as true"
 we can do this, because we don't have to limit our first statement in conditional
 to taking something of type 'bool' : b/c we're in a dynamically-typed language,
 we can take any type of initial arg |#

; example with 

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

(define (factorial-normal x)
  (if (= x 0)
      1
      (* x (factorial-normal (- x 1)))))

(define (factAcc x)
  (let helper ([x x] [acc 1])
    (if (= x 0)
        acc
        (helper (- x 1) (* acc x)))))


