#lang racket

(define ( pow a b )
      (if (zero? b) 1 (* a (pow a (sub1 b)))))
 
(define (trailing-zeros-h n)
       (define( helper curr-k prod)
       ( if (> curr-k n) prod ( helper (* curr-k 5) (+ prod (quotient n curr-k)))))
      (helper 5 0)
   
  )

