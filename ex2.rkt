#lang racket
 
(define (GCD x y ) (cond
                     [ ( = x 0 ) y]
                     [ ( = y 0) x]
                     [ (> x y ) (GCD x (remainder x y ))]
                     [ (< x y ) (GCD y ( remainder y x))]))

 
(define( divisor x ) ( define (divisorH x n )( if (= (remainder x n) 0) n (divisorH x (- n 1 )))  )
  (divisorH x (- x 1)))
 

(define (sum_odds a b )(define ( helper a1 b1) (
                       if (>= a1 b1 ) b1 ( + a1  (helper (+ a1 2 ) b1))))
                           ( cond
                           [ (= (remainder a 2) 0) (helper (+ a 1 ) b)]
                           [(= (remainder a 2) 1) (helper  a   b)]))
 
(define (prime x ) (if
                    (= x 1 )
                    #f
                    (= (divisor x ) 1)))
(define (reverse x) ( define (helper result leftover)
                        ( if (zero? leftover )
                             result
                             (helper (+ (* result 10) (remainder leftover 10)) ( quotient leftover 10))))
                             (helper 0 x))
 
(define (pol x)
  (if (< x 10)
      #f
      (= x (reverse x ))))
 
( define (sum x y )
   (define (helper1 x1 y1 cnt )
                      ( cond
                        [ (> x1 y1 ) cnt ]
                        [ (pol x1 ) ( helper1 (+ x1 1 ) y1 ( + cnt 1))]
                        [ ( not ( pol x1 )) (helper1 (+ x1 1 ) y1  cnt )])) (helper1 x y 0))
 
(define (count-divisors x)
  ( define ( helper2 x1 i)(
                           if
                           (= (divisor x1) 1 ) i
                           ( helper2 (divisor x1) (+ i 1))))
                           (helper2 x 2)) 