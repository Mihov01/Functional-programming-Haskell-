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
  


((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") 
("HKO" . "ORD"))) "YUL")
(equal? ((itinerary '(("G" . "E") ("E" . "F") ("A" . "G") ("D" . "E") ("B" . "C") ("E" . "C") ("A" . "B") ("C" . "D") ("F" . "A") ("C" . "A"))) "A") '("A" "B" "C" "A" "G" "E" "C" "D" "E" "F" "A"))   
 
        