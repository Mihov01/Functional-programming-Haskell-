#lang racket
(define (lenght x) ; returns the lenght of a number 
  ( define (lenght_helper x1 itter)(
    cond
     [(zero? x1) itter]
     [else (lenght_helper (quotient x1 10) (+ itter 1))]))
      (lenght_helper x 0))

( define (pow x n) ; a function that returns x to the power of n
   (cond
      [(zero? n) 1]
      [(= n 1) x ]
      [else (* x (pow x (- n 1 )))]))
(define (add-ones n ) (
                       define (add-ones-helper leftover result lenght-num) ; helper that has 3 arguments , a temporary number leftofer , the last digit of witch we use on every itteration
                        ; result , which stores the result and lenght-num which stores the lenght of the result on every itteration
                        ( cond
                           [(< n 0) (error "n should be positive")]; if the user inputs invalid argument 
                           [(zero? leftover ) result]
                           [else (add-ones-helper (quotient leftover 10) ; remove the last digit of the leftover
                                                  (+(+(* (+(remainder leftover 10) 1) (pow 10 lenght-num)) ) result ) ; adds the last digit on it's place increased by 1 it works this way:
                                                  ; we use 10 to the power of the lenght of the whole new num to get our increased number to its desired place for example if we had
                                                  ; the desired number to be 5 and the previous number was 46 , we would multiply 5 by 10 to the power of 3 (the lenght of the new number ) and then we would add 46 
                                                  (+ lenght-num (lenght (+(remainder leftover 10) 1))))])) ; the lenght is increased by the lenght of the added digits
                           (add-ones-helper n 0 0))


