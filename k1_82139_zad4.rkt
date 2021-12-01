#lang racket

(define (prod n )
     ( define (helper curr-n prod1)
            ( if (zero? curr-n) prod1 (helper (quotient curr-n 10) (* prod1  (remainder curr-n 10)))))
 ( helper n 1))
(define (lst x)
     ( define ( helper curr-x new-lst)
            (if (zero? (quotient curr-x 10))
                (append new-lst (list (remainder curr-x 10)))
                (helper (prod curr-x) (append new-lst (list curr-x)))))
  (helper (prod x) '()))

(define (persistence n)
     (cons (lst n ) (length( lst n))))
