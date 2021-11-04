#lang racket
(define (my-compose f g)
  (λ (x)
    (f (g x))))

(define (repeated f n)
        ( define( helper f1 curr-n)
        (if (zero? curr-n)
              f1
              (helper ( λ (x) (f (f1 x))) (- curr-n 1 ))))
         (helper f n))


(define (repeated-1 f n )
        ( define (helper f1 curr-n )
          ( if (zero? curr-n )
               f1
           (helper (my-compose f f1 ) (- curr-n 1 ))))
        (helper f n))
;(define (test a)
 ;       (+ a 5))
;((repeated test  5) 2)
;((repeated-1 test  5) 2)
(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

(define (derive-x f eps )
   (λ(x y) (/ (-(f (+ x eps) y) (f x y )) eps )))
(define (derive-y f eps )
   (λ(x y) (/ (-(f  x (+ y eps)) (f x y )) eps )))

(define (length-list list)
  (cond
    [(empty? list)  0]
    [(cons? list)   (+ 1 (length-list (rest list)))]))

;(length-list '(1 2 3 4 5 6))

(define (inside? list a)
  (cond
    [(empty? list) #f]
    [ (= ( first list )a) #t]
    [else (inside? ( rest list ) a)]))
(inside? '(1 2 3 4 5) 6)
