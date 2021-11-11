#lang racket


(define (length lst)
        ( define (helper cnt curr-lst )
        ( if (null? curr-lst) cnt (helper  (+ cnt 1) ( cdr curr-lst))))
        (helper 0 lst))
;(length '(1 2 3 4))
;(length '())

(define (inside? lst obj)
        ( cond
           [(null? lst)  #f]
           [(= (car lst ) obj ) #t]
           [ else (inside? (cdr lst) obj)]))

;(inside? '(1 2 3 4) 1)
;(inside? '(1 2 3 4) 5)

(define (add  obj pos lst )
    ( define (helper curr-lst curr-pos new-lst)
             (cond
               [(>= pos (length lst)) (append curr-lst (list obj))]
               [(> curr-pos pos) ( append new-lst  (list obj) curr-lst)]
               [ else (helper ( rest curr-lst ) (+ curr-pos 1) (append new-lst (list (first curr-lst))))]))
            (helper lst 1 '()))

;(add  1 2 '(1 3 4 5))


( define (smallest lst)
   
       (define ( helper curr-s curr-lst)
             (cond
               [(null? curr-lst) curr-s ]
               [(< (first curr-lst) curr-s) (helper ( first curr-lst ) (rest curr-lst))]
               [else (helper curr-s (rest curr-lst))]))
               (if (empty? lst)
                     (error "Invalid input")
                            (helper (first lst) (rest lst))))
;(smallest '(1 2 3 -4 5 6))
  (define ( erase obj lst )
          (define ( helper new-lst curr-lst )
                  ( cond [ ( empty? curr-lst) new-lst]
                         [ (equal? (first curr-lst) obj) ( append new-lst (rest curr-lst))]
                         [ else (helper (append new-lst (list (first curr-lst))) (rest curr-lst))]))
                        
                                  (helper '() lst))

( define ( erase-all  obj lst)
         (define ( helper curr-lst new-lst)
                 ( cond [(empty? curr-lst) new-lst]
                        [(equal? (first curr-lst) obj)  (helper (rest curr-lst) (erase obj new-lst))]
                        [else                           (helper (rest curr-lst)  new-lst)]))
                 (helper lst lst))
;(erase-all 3 (erase-all 5 '( 1 2 3 5 3 3 3 5 5 5 5 53 3)))
(define (reverse lst1 )
        (define ( helper  new-lst curr-lst)
               ( if (empty? curr-lst)
                     new-lst
                     (helper ( append new-lst (list (last curr-lst))) (drop-right curr-lst 1))))
                      (helper '() lst1))
;(reverse '(1 2 3 4))