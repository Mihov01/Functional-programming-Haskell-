#lang racket

(define (find-d x)
  (define (find-d-helper temp itter)
    ( cond
       [ ( zero? temp ) itter]
       [ (zero? (remainder x temp))
                (find-d-helper (- temp 1 ) (+ itter 1))]
       [ else (find-d-helper (- temp 1 ) itter)]) )
       (find-d-helper (- x 1) 0) )

(define (perfect? x)
  ( = x (find-d x)))

(define (inc-digits? x)
  ( define (inc-helper leftover-x cur-dig)
     ( cond
        [(zero? leftover-x)  ]
        [(> (remainder leftover-x 10) cur-dig) #f]
        [else (inc-helper (quotient leftover-x 10) (remainder leftover-x 10))]))
        (inc-helper (quotient x 10) (remainder x 10)))
(define (pow x n)( if (zero? n ) 1 (* x (pow x (- n 1 )))))

(define (find-sum x n) (
                        define (find-sum-helper curr-sum curr-pow)
                         ( cond
                            [(> curr-pow n) curr-sum]
                            [else (find-sum-helper (+ curr-sum (pow x curr-pow)) (+ curr-pow 1))]))
                           (find-sum-helper 1 1))

(define (find-sum-itter x n) (
                        define (find-sum-itter-helper curr-sum curr-number curr-pow)
                         ( cond
                            [(> curr-pow n) curr-sum]
                            [else (find-sum-itter-helper (+ curr-sum curr-number) (* x curr-number) (+ curr-pow 1))]))
                           (find-sum-itter-helper 1 x 1))

                  