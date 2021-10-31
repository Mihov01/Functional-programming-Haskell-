#lang racket
(define (my-negate p?) 
        ( λ (x)
           (not (p? x))))

(define (my-curry f x)
                  (λ (y z) (f x y z)))

(define (difference F a b)
        ( - (F a) (F b)))

(define f2 ( λ (x) (* 2 x )))
  ; f'(x) =   lim     (f(x + eps) - f(x)) / eps
;         eps -> 0

(define (derive func eps)
       (λ (x) (/ (- (func (+ x eps)) (func x)) eps )))
;((derive (λ (x) (* 4 x x)) 1e-2) 1)
;((derive (λ (x) (* 4 x x)) 1e-2) 2)

(define (derive2 func eps)
  (derive (derive func eps ) eps))

((derive2 (λ (x) (* 4 x x)) 1e-2) 1)
((derive2 (λ (x) (* 4 x x)) 1e-2) 2)

(define (derive-n f n eps )
     (define (helper f-curr curr-n)
       (cond
         [(zero? curr-n) f-curr]
         [else (helper (derive f-curr eps ) (- curr-n 1 ))]))
       (helper f n))
((derive-n (λ (x) (* 4 x x)) 0 1e-2) 2)
((derive-n (λ (x) (* 4 x x)) 1 1e-2) 2)
((derive-n (λ (x) (* 4 x x)) 2 1e-2) 2)
((derive-n (λ (x) (* 4 x x)) 3 1e-2) 2)