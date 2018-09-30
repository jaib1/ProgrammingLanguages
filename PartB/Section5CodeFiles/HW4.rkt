
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

#| this makes sure to use TCO for compute optimization
as opposed to a shorter function which doesn't use TCO |#

(define (sequence low high stride)
  (letrec ([helper (lambda (low2 high2 stride2 acc)
                     (if (> low2 high2)
                         (reverse acc)
                         (helper (+ low2 stride2) high2 stride2 (cons low2 acc))))])
    (helper low high stride null)))

(define (string-append-map xs suffix)
  (map (lambda (hdxs) (string-append hdxs suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (let ([i (remainder n (length xs))])
           (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (s2 n2 acc)
                     (cond [(= n2 0) (reverse acc)]
                           [#t (helper (cdr (s2)) (- n2 1) (cons (car (s2)) acc))]))])
    (helper s n null)))

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (* -1 x) (lambda () (f (+ x 1))))]
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cond [( = (remainder x 2) 0) (cons "dog.jpg" (lambda () (f (+ x 1))))]
                      [#t (cons "dan.jpg" (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (f s)))

(define (cycle-lists xs ys)
  (letrec ([helper (lambda (n)
                     (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () helper (xs ys (+ n 1)))))])
    (helper xs ys 0)))

(define (vector-assoc v vec)
  (letrec  ([len (vector-length vec)] ;create variable "len" for overall length of the vector "vec"
            [helper (lambda(cur) ;
                 (cond[(= (+ cur 1) len) #f] ;if 
                      [(pair? (vector-ref vec cur)) (if (equal? (car (vector-ref vec cur)) v)
                                                            (vector-ref vec cur)
                                                            (f (+ cur 1)))]
                      [#t (f (+ cur 1))]))])
    (helper 0)))
         

