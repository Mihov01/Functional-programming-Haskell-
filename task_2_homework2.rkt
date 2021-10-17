#lang racket

(define (sum_of_digits_itter x)
  ( define (helper curr itter)
     (cond
       [ (negative? curr) (error "negative number")]
       [ (zero? curr) itter]
       [else (helper (quotient curr 10) (+ itter 1 ))]))
  (helper x 0))