#lang racket
(define (switchsum f g n)
     (define ( helper res curr-n x )
            (cond [(= curr-n n ) res ]
                  [( zero? (remainder curr-n 2 )) (helper (+ res (f x) ) (add1 curr-n) (f x) )]
                  [else (helper (+ res (g x) ) (add1 curr-n) (g x))]))
    (λ (x) (helper 0 0  x )))
       ((switchsum (lambda (x) (+ x 1)) 
 (lambda (x) (* x 2)) 1) 2);→ 3
((switchsum (lambda (x) (+ x 1)) 
 (lambda (x) (* x 2)) 2) 2); → 9
((switchsum (lambda (x) (+ x 1)) 
 (lambda (x) (* x 2)) 3) 2) ;→ 16
((switchsum (lambda (x) (+ x 1)) 
 (lambda (x) (* x 2)) 4) 2) ;→ 30


(define ( sum-digits number)
       (define (helper res curr-n d)
               (if (and ( zero? curr-n) (zero? d )) res ( helper (+ res d ) (quotient curr-n 10) (remainder curr-n 10))))
       (helper 0 (quotient number 10) (remainder number 10)))

(define (sum-sum-digit a b k)
      (define ( helper res curr-a)
            ( cond [(> curr-a b ) res]
                   [(zero? (remainder (sum-digits curr-a) k)) (helper (+ res curr-a) (add1 curr-a))]
                   [else                                      (helper  res  (add1 curr-a))]))
      (helper 0 a))
(sum-sum-digit 1 20 7)
(define (maxOrderedSublist xs)
  (define (helper cur-el lst cur-lst max-sub-lst)
    (cond [(empty? lst) max-sub-lst]
          [(< cur-el (car lst)) (helper (car lst) (cdr lst) (cons (car lst) cur-lst) max-sub-lst)]
          [else (if (> (length cur-lst) (length max-sub-lst))
                    (helper (car lst) (cdr lst) (list (car lst)) cur-lst)
                    (helper (car lst) (cdr lst) (list (car lst)) max-sub-lst))]))
  (if (empty? xs)
      '()
      (reverse (helper (car xs) xs (list (car xs)) (list (car xs))))))

(maxOrderedSublist '(1 2 3 4 1 2 0 1 2 3 4 5 0)) ; -> '(0 1 2 3 4 5)
(maxOrderedSublist '(1 2 3 4 1 2 0 1 2 0 3 4 5 0)) ; -> '(1 2 3 4)