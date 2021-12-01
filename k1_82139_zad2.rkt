#lang racket

(define (lengths-l lst)
     ( map length lst))


(define (get-missing-length  xs)
     ( define (helper curr-xs)
              ( cond [(null? (cdr curr-xs)) #f]
                     [(not(=(add1 (car curr-xs) ) (second curr-xs))) (add1 (car curr-xs) )]
                     [ else (helper (cdr curr-xs))]))
  (if (null? xs) (error "Empty list!")  
     ( helper (sort (lengths-l xs) <))))
#|(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9)))
(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a", 
"a") ("a") ("a", "a", "a", "a", "a", "a")))
(get-missing-length '())|#