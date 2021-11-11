#lang racket
(define (itinerary lst )
(define (find-match lst1 obj best str );retuens the pair where the object is the first of the two , if there is no such pair returns null list
        (cond
          [(null? lst1 ) best]
          [ (and (equal? (car (car lst1 )) obj ) ( equal?  str " ")) (find-match ( cdr lst1 ) obj (car lst1 ) (cdr (car lst1 )) ) ]
          [ (and (equal? (car (car lst1 )) obj ) ( string<? (cdr (car lst1 )) str)) (find-match ( cdr lst1 ) obj (car lst1 ) (cdr (car lst1 )) )  ]
          [ else  (find-match ( cdr lst1 ) obj best str )]))

(define (find-sequence lst2 obj)
        (define ( helper curr-list curr-obj new-list)
        (cond
            [(or (null? curr-list) (null? (find-match curr-list curr-obj '() " "))) new-list]
            [ else (helper
                    ( remove (find-match curr-list curr-obj '() " ") curr-list)
                    ( cdr (find-match curr-list curr-obj '() " "))
                    ( append new-list (list (cdr (find-match curr-list curr-obj '() " ")))))]))
        (helper lst2 obj (list obj)))
  
   (λ (x) ( cond
             [ ( <= ( length (find-sequence lst x)) (length lst)) (error "No such itinerary!")]
             [else (find-sequence lst x)]))) 
  


((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") 
("HKO" . "ORD"))) "YUL")
(equal? ((itinerary '(("G" . "E") ("E" . "F") ("A" . "G") ("D" . "E") ("B" . "C") ("E" . "C") ("A" . "B") ("C" . "D") ("F" . "A") ("C" . "A"))) "A") '("A" "B" "C" "A" "G" "E" "C" "D" "E" "F" "A"))   
 
        