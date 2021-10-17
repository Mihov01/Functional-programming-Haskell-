#lang racket

(define (find_sum_of_digits x)
  ( define (helper_sum res cur )
     ( if (zero? cur)
          res
          ( helper_sum ( + res (remainder cur 10)) (quotient cur 10))) )
  (helper_sum 0 x))
