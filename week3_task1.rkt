#lang racket
(define (remove-first-occurrence num digit )
        (define (remove-first-occurrence-helper  curr-num place )
        (cond
          [( zero? curr-num) num] ; ако няма такава цифра връща оригинала 
          [(and ( zero? (quotient curr-num 10)) (=(remainder curr-num 10) digit )) (remainder num (expt 10 (- place 1 )))] ;ако търсеното  число е на 1ва позиция връща числото без 1вата цифра
          [(and(=(remainder curr-num 10) digit ) (= place 1 )) (quotient num 10)] ; ако числото е на последна цифра
          [(=(remainder curr-num 10) digit ) (+(*(quotient curr-num 10) (expt 10 (- place 1))) (*(remainder num (expt 10 (- place 1)))))] ; ако намери числото връща цифрите преди числото * по броя на другите цифри без числото умножени по 10 и добавя към тях останалите цифри
          [else (remove-first-occurrence-helper  (quotient curr-num 10) (+ place 1 ))])); ако не е числото продължава нататък
          (remove-first-occurrence-helper  num 1 ))

; the second version removes all the occurences of the digit in the number
(define (remove-first-occurrence-1 num digit )
( define (remove-first-occurrence-helper-1 result left-over curr-place )
   ( cond
      [(zero? left-over ) result]
      [(=(remainder left-over 10) digit ) (remove-first-occurrence-helper-1 result (quotient left-over 10)  curr-place )]
      [else (remove-first-occurrence-helper-1 (+(*(remainder left-over 10) (expt 10 (- curr-place 1))) result) (quotient left-over 10) (+ curr-place 1))]))
      (remove-first-occurrence-helper-1 0 num 1))



(= (remove-first-occurrence 153565 5) 15356) 
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence-1 1212 1) 1) 22)


(= (remove-first-occurrence-1 153565 5) 136) 
(= (remove-first-occurrence-1 15360 0) 1536)
(= (remove-first-occurrence-1 15300 0) 153)
(= (remove-first-occurrence-1 15365 1) 5365)
(= (remove-first-occurrence-1 35365 3) 565)
(= (remove-first-occurrence-1 1212 1) 22)
(= (remove-first-occurrence-1 1212 2) 11)
(= (remove-first-occurrence-1 (remove-first-occurrence-1 1212 1) 1) 22)