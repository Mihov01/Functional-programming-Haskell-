#lang racket
(define (itinerary lst )
(define (find-match lst1 obj best str );returns the pair where the object is the first of the two , if there is no such pair returns null list lst1=== the lidt ; obj === the desired match ,
  ; best == best pair found ; str== the second part of the best found pair , used to compare if there is another match
        (cond
          [(empty? lst1 ) best]
          [ (and (equal? ( caar lst1 ) obj ) ( equal?  str " ")) (find-match ( cdr lst1 ) obj (car lst1 ) (cdr (car lst1 )) ) ]; cathes the first case 
          [ (and (equal? (caar lst1 ) obj ) ( string<? (cdr (car lst1 )) str)) (find-match ( cdr lst1 ) obj (car lst1 ) (cdr (car lst1 )) )  ]; if the found match is better than the current best
          [ else  (find-match ( cdr lst1 ) obj best str )]))

(define (find-sequence lst2 obj)
        (define ( helper curr-list curr-obj new-list)
       ( if  (empty? (find-match curr-list curr-obj '() " ")) ; if there is no more matches 
                       new-list 
                           (helper   ( remove (find-match curr-list curr-obj '() " ") curr-list)
                                     ( cdr (find-match curr-list curr-obj '() " "))
                             ( append new-list (list (cdr (find-match curr-list curr-obj '() " ")))))))
                      (helper lst2 obj (list obj)))
  
   (λ (x) ( if ( <= ( length (find-sequence lst x)) (length lst))
                    (error "No such itinerary!")
                    (find-sequence lst x))))
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

;((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") 
;("HKO" . "ORD"))) "YUL")
;((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A"))) 
;"A")
;((pad '( (1 2 3)
; (4 5 6)
; (7 8 9) )
;) 0)
;((pad '( (1 2 3)
; (4 5 6)
; (7 8 9) )
;) 9)