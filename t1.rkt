#lang racket


(define v '( 1 2 3 4 5 6 1 2 3 1 5))

(append (list (car v )) (filter (λ (x) ( not (equal? x (car v)))) (cdr v)))
(take '( 1 2 3 4 5) 0)
(define (sublistBetween start end xs)
     (if (=  start 0 ) (take xs end)
       (drop (take xs end) (- start 1 ))))


(sublistBetween 0 6 '(1 2 3 4 5 6 7 8 9))

(define (removeDuplicates xs)
  (define (helper new-lst itter  e)
        (cond [(and (= itter  (length new-lst)) ( equal? (last new-lst) (last (take new-lst (- itter 1 ))))) (take new-lst (- itter 1 ))]
              [(> itter  (length new-lst)) new-lst]
              [else (helper (append (take new-lst  itter  ) ( filter (λ (x) (not (equal? e x))) (drop  new-lst itter))) (+ itter 1 )  (last (take new-lst itter )))]))
        (helper  xs 1  (first xs)))
(removeDuplicates '(1 2 3 2 4 2 5 5 2 1 ))


