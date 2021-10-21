#lang racket
( define (max-multiple d b)
   (define (max-multiple-helper curr-n)
     (cond
       [(zero? curr-n) (error "No such element found")]
       [(zero? (remainder curr-n d)) curr-n]
       [ else ( max-multiple-helper (- curr-n 1))]))
          (max-multiple-helper b)) 