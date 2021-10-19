#lang racket

(define (count-itter-num x y ) ;finds the occurrences of a digit(y) in a number(x)
  ( define (count-itter-num-helper x1 itter)
     ( cond
        [ (zero? x1) itter]
        [ (= (remainder x1 10) y) (count-itter-num-helper (quotient x1 10 ) (+ itter 1 ))];if the last digit of the temporary number x1 is equal to the desired digit we add 1 to the itter and discard the last digit of the temp
        [else (count-itter-num-helper (quotient x1 10 )  itter )])); if it does not we discard the last degit
  (count-itter-num-helper x 0))

(define (total-itter-count x y ) ; we find the number of occurrences of a digit(y) in a given interval [1,x] 
  ( define (total-itter-count-helper x1 itter)
     ( cond
        [(< x 0 ) (error "the number should be >0")] ; if the user inputs invalid argument
        [(zero? x1) itter] ; if we go out of bounds  return the itter 
        [else (total-itter-count-helper (- x1 1) (+ itter (count-itter-num x1 y)))])); begins with the last number of the interval and on every itteration we use a number lower
  (total-itter-count-helper x 0))

( define (sum-of-numbers x); a simple function that returns the sum of the digits of a given number 
   ( define (sum-of-numbers-helper x1 itter)
( cond
   [ (zero? x1) itter]
   [else (sum-of-numbers-helper (quotient x1 10) (+ itter (remainder x1 10)))]))
      (sum-of-numbers-helper x 0))

(define (sum-counts-iter x d) (
                   sum-of-numbers (total-itter-count x d)))



