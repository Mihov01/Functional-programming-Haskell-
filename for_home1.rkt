#lang racket
(define (my-cartesian-product lst1 lst2 )
        (define ( helper curr-lst1 curr-lst2 res )
                (cond
                  [(empty? curr-lst1) res]
                  [(empty? curr-lst2) (helper (cdr curr-lst1) lst2 res)]
                  [else (helper  curr-lst1 (cdr curr-lst2)  (append res (list (cons (car curr-lst1) (car curr-lst2)))))]))
       (helper lst1 lst2 '()))

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))



(define (my-reverse-iter xs )
        (define (helper curr res )
            (if (empty? curr) res ( helper (rest curr) (cons   (car curr) res))))
       ( helper xs '()))
(my-reverse-iter '(1 2 3 4 5))

(define (where lst pred-lst )
      ( define (helper curr-l curr-lp res )
         (cond [(empty? curr-l ) res ]
               [ (empty? curr-lp)  (helper (cdr curr-l) pred-lst (append res (list(car curr-l))))] 
               [(not ((car curr-lp) (car curr-l))) (helper (cdr curr-l) pred-lst res )]
               [else (helper curr-l (cdr curr-lp)  res )]))
       (helper lst pred-lst  '()))

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))                

