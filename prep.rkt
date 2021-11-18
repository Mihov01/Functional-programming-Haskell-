#lang racket
(define (my-flatten lst )
(cond 
     [(null? lst )  lst]
     [ (list? (car lst)) (append (my-flatten( car lst)) (my-flatten ( cdr lst)))]
     [ else (cons (car lst ) (my-flatten (cdr lst) ))]))
(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))

(define (sum-digit-divisors x )
   (define (helper curr-x d )
  ( cond
        [ (and (zero? curr-x) (zero? d)) 0]
        [ (zero?  d ) (+ 0 (helper (quotient curr-x 10) (remainder curr-x 10)))]
        [ (zero? (remainder x d ))  (+ d (helper (quotient curr-x 10) (remainder curr-x 10)))]
        [else (+ 0 (helper (quotient curr-x 10) (remainder curr-x 10)))]))
    (helper (quotient x 10 ) ( remainder x 10 )))
(= (sum-digit-divisors 1) 1)
(= (sum-digit-divisors 28) 2) 
(= (sum-digit-divisors 32) 2)
(= (sum-digit-divisors 29) 0)
(= (sum-digit-divisors 34) 0)
(= (sum-digit-divisors 1048) 13)