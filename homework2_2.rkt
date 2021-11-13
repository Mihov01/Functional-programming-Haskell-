#lang racket
( define ( pad xs)
(define l-sub-lst (if (empty? xs) (error "Empty matrix") (length (first xs)))) ; returns the lensizeght of one row
(define l-lst (length xs))  ; return the count of rows 
(define ( number-list n c) ; returns a list of  n (numbers) with count of c for example (number-list 0 5 ) returns '(0 0 0 0 0) 
        (define (helper curr-c lst1 )
         (if (zero? curr-c )
             (list lst1)
               ( helper (- curr-c 1 ) (append lst1 (list n)))))
         (helper c '()))

(define (add-number lst n ) ; adds a number before  the first place and after the last place of a list
    (list(list (append (list n) (append lst (list n)) ))))

(define (add-numbers lst n )
        ( define ( helper l curr-l new-lst cnt-el curr-lst  ) ; l== lenght of list curr-l == temporary lenght of new list ; cnt-el === count of elements in a row 
           (cond [(> curr-l (+ l 1)) new-lst]
                 [(= curr-l  (+ l 1)) (helper l (+ curr-l 1 ) (append new-lst  (number-list n (+ cnt-el 2 ))) cnt-el  curr-lst)]
                 [ else (helper  l (+ curr-l 1 ) (append new-lst (add-number (first curr-lst) n)) cnt-el (rest curr-lst))]))
        (helper l-lst 1 ( number-list n (+ l-sub-lst 2) ) l-sub-lst lst))
     (λ (x) ( add-numbers xs x))
   )



