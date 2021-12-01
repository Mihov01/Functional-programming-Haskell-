#lang racket

#|(define (transpose xs)
        ( apply map list xs))
( define (zero-rows xss)
        ( map (λ (xs) ( if (ormap zero? xs)  (map (λ (x) 0) xs) xs)) xss ))
;(zero-rows '((1 0 3) (2 4 5) (6 7 8)))
(define (zero-columns xs)
      (transpose (zero-rows (transpose xs))))

 (define cube ( λ (x) (* x x x))) === (define (cube x) ( * x x x))
(define (sum-cubes a b)|#

(define (find-max xs)
    (define (helper1 lst new-lst)
           (cond [(empty? lst) new-lst]
                 [ (> (last new-lst) (first lst)) new-lst]
               [else ( helper1 (cdr lst) (append new-lst (list (car lst))))]))


     (define (helper2 curr-lst best-lst)
         (cond [(empty? curr-lst) best-lst]
               [(> (length (helper1 (cdr curr-lst) (list(car curr-lst)))) (length best-lst))
                (helper2 (cdr curr-lst)  (helper1 (cdr curr-lst) (list(car curr-lst))))]
               [ else (helper2 (cdr curr-lst) best-lst)]))

     (helper2 xs '()))
 (find-max '(1 5 2 2 4 6 8 3 4 1))


 (define (where lst lst1)
     (filter (λ (a)(andmap (λ (x) ( x a)) lst1)) lst))
(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5))))


(append '(a (b c)) (caddr '((a b) c ((d) e))))

(cons '(a b) (list 'c '((d) e)))
(map length
 	(map (lambda (x)
 (cond [(not (pair? x)) (list x)]
 [(null? (cdr x)) x]
[else (cdr x)]))
 '((2 3 4) (8 5) 6 (7 1 –1 5) (1))))

(define (my-flatten xs )
     ( map ( 

  