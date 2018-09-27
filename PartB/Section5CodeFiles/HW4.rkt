
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

#| this makes sure to use TCO for compute optimization
as opposed to a shorter function |#

(define (sequence low high stride)
  (let helper ([low2 low] [high2 high] [stride2 stride] [acc null])
    (if (> low2 high2)
        (reverse acc)
        (helper (+ low2 stride2) high2 stride2 (cons low2 acc)))))
  