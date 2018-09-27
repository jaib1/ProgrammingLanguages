
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
         (let ([i (remainder n (- (length xs) 1))])
           (car (list-tail xs i)))]))
        

  