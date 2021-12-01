#lang racket

(define square (λ (x) (* x x)))
(square 123)
(define (occur n d)
     ( define ( helper  cnt curr-n d1)
            (cond [(and (zero? curr-n) (zero? d1)) cnt]
                  [(= d d1) (helper (add1 cnt) (quotient curr-n 10)(remainder curr-n 10))] 
                  [else (helper  cnt (quotient curr-n 10) (remainder curr-n 10))]))
    (helper 0 (quotient n 10) (remainder n 10)))


(define (distribution n)
        (define (helper curr-n new-lst)
          (cond [(zero? curr-n)  new-lst]
                [(equal? (assoc (remainder curr-n 10) new-lst) #f)
                 ( helper (quotient curr-n 10)
                               (append new-lst (list (cons (remainder curr-n 10) (occur n (remainder curr-n 10))))))]
                [ else( helper (quotient curr-n 10)
                                new-lst )]))
      (helper n '()))
(define (get-distribution n)
        ( sort (distribution (square n)) (λ (x y) (< (car x) (car y)))))

