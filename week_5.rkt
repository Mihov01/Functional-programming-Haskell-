#lang racket


(define (substr?  a b)
        (define (lenght num)
        (define (lenght-helper curr-n lenght-num)
        (if (zero? curr-n)
             lenght-num
            (lenght-helper (quotient curr-n 10) (+ lenght-num 1))))
            (lenght-helper num 0) )
        (define (substr-helper leftover temp-n lenght-a)
        (cond
          [(zero? leftover ) #f]
          [(= a temp-n) #t]
          [else (substr-helper
                (quotient leftover 10)
                (remainder leftover lenght-a)
                lenght-a)]))
        (substr-helper
        (quotient b 10)
        (remainder b (expt 10 (lenght a)))
        (expt 10 (lenght a))))

;(equal?(substr? 123 5123783) #t)
;(equal? (substr? 123 123783) #t)
;(equal? (substr? 123 12373) #t)
;(equal? (substr? 123 5123) #t)
;(equal? (substr? 123 123) #t)
;(equal? (substr? 123 513783) #f)
;(equal? (substr? 123 12) #f)

(define (my-identity x)x)
(define (my-compose f g)
        (λ (x) (f (g x))))






