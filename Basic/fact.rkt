#lang racket
(define (fact n )
  ( if (= n 1 )
       1
       (* n (fact (- n 1)))))

(define (fact_itter x ) (
                         define (helper res left)
                          ( if (zero? left ) res (helper (* res left ) (- left 1))))
  
                             (helper 1 x))                      
  
