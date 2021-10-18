#lang racket

(define (func x y )
  ( define (helper x1 itter)
     ( cond
        [ (zero? x1) itter]
        [ (= (remainder x1 10) y) (helper (quotient x1 10 ) (+ itter 1 ))]
        [else (helper (quotient x1 10 )  itter )]))
  (helper x 0))
(define (func2 x y )
  ( define (helper2 x1 itter)
     ( cond
        [(< x 0 ) (error "the number should be >0")]
        [(zero? x1) itter]
        [else (helper2 (- x1 1) (+ itter (func x1 y)))]))
  (helper2 x 0))
( define (func3 x)
   ( define (helper3 x1 itter)
( cond
   [ (zero? x1) itter]
   [else (helper3 (quotient x1 10) (+ itter (remainder x1 10)))]))
      (helper3 x 0))
(define (final_func x y) (
                   func3 (func2 x y)))