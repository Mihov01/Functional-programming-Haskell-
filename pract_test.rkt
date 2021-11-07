#lang racket
(define (sum-numbers x )
  (define (helper sum  curr-n )
    ( if (zero? curr-n)
           sum
             (helper (+ sum ( remainder curr-n 10 )) (quotient curr-n 10))))
  (helper 0 x))

(define (order? x)
   (define (helper curr-n d)
   ( cond
      [(zero? curr-n ) #t ]
      [(< (remainder curr-n 10) d ) #f]
      [else (helper (quotient curr-n 10) (remainder curr-n 10))]))
  (helper (quotient x 10)  (remainder x 10)))

(define (sum-numbers-1 a b )
  (define (helper curr-n sum )
    (cond
      [(> curr-n b) sum]
      [(order? curr-n) (helper (+ curr-n 1) (+ sum  curr-n))]
      [else (helper (+ curr-n 1)  sum)]))
    (helper a 0))
 (sum-numbers-1 1 9 )
(sum-numbers-1 199 203 )
(sum-numbers-1 219 225 )