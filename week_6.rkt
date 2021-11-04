#lang racket
(define (my-compose f g)
  (λ (x)
    (f (g x))))

(define (repeated f n)
        ( define( repeated-helper f1 curr-n)
        (if (zero? curr-n)
              f1
            (repeated-helper ( λ (x) (f (f1 x))) (- curr-n 1 ))))
           (repeated-helper f n))


(define (repeated-1 f n )
        ( define (repeated-1-helper f1 curr-n )
          ( if (zero? curr-n )
               f1
           (repeated-1-helper (my-compose f f1 ) (- curr-n 1 ))))
        (repeated-1-helper f n))


(define (derive-x f eps )
   (λ(x y) (/ (-(f (+ x eps) y) (f x y )) eps )))
(define (derive-y f eps )
   (λ(x y) (/ (-(f  x (+ y eps)) (f x y )) eps )))
;(define (test a)
 ;       (+ a 5))
;((repeated test  5) 2)
;((repeated-1 test  5) 2)
(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

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
;(inside? '(1 2 3 4 5) 6)

(define (newton-sqrt x)
 (define (newton-method-helper g y)
  (- y (/ (g y) ((derive g 1e-2) y))))
  (define (itter n fn)
    (if
     (zero? n)
       fn
      (newton-method-helper (λ (x0) (+ (* x0 x0) fn)) fn) ))
    ( itter 10 x))



(define (sum-digit-divisors n)
        (define (sum-digit-divisors-helper curr-num digit sum)
        ( cond
           [(and (zero? curr-num) (zero? digit)) sum]
           [(zero? digit) ( sum-digit-divisors-helper (quotient curr-num 10) (remainder curr-num 10) sum)]
           [(= (remainder n digit) 0) ( sum-digit-divisors-helper (quotient curr-num 10) (remainder curr-num 10) (+ sum digit))]
           [else  ( sum-digit-divisors-helper (quotient curr-num 10) (remainder curr-num 10) sum)]))
        ( sum-digit-divisors-helper  (quotient n 10)  (remainder n 10) 0))

       (sum-digit-divisors 29)
       (sum-digit-divisors 34)
       (sum-digit-divisors 28)
       (sum-digit-divisors 12222)


