#lang racket
( define ( pad lst)  
(define ( number-list n c)
        (define (helper curr-c lst1 )
         (if (zero? curr-c )
             (list lst1)
               ( helper (- curr-c 1 ) (append lst1 (list n)))))
         (helper c '()))

(define (add-number lst n )
    (list(list (append (list n) (append lst (list n)) ))))

(define (add-numbers lst n )
        ( define ( helper l curr-l new-lst cnt-el curr-lst  )
           (cond
              [(> curr-l (+ l 1)) new-lst]
              [(= curr-l  (+ l 1)) (helper l (+ curr-l 1 ) (append new-lst  (number-list n (+ cnt-el 2 ))) cnt-el  curr-lst)]
              [ else (helper  l (+ curr-l 1 ) (append new-lst (add-number (car curr-lst) n)) cnt-el (cdr curr-lst))]))
        (helper (length lst) 1 ( number-list n (+ (length (car lst )) 2) )  (length (car lst )) lst))
     (λ (x) ( add-numbers lst x))
   )

(define (list-string lst)
       (define (helper curr-lst str cnt )
         (cond
           [(zero? cnt) str]
           [(= cnt 1 ) (helper (cdr curr-lst) (string-append str " " (number->string (car curr-lst)) ")") (- cnt 1 ))]
           [else (helper (cdr curr-lst) (string-append str " " (number->string (car curr-lst))) (- cnt 1 ))]))
     (helper (cdr lst) (string-append "(" (number->string (car lst))) (- (length lst) 1))) 


((pad '( (1 2 3)
 (4 5 6)
 (7 8 9) )
) 0)

