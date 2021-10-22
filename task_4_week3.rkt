#lang racket
( define (calc-series-sum x n)
   ( define (calc-series-sum-helper curr-sum curr-n  prev-den curr-den)
      ( cond
         [(> curr-n n) curr-sum]
         [(zero? ( remainder curr-n 2))
          (calc-series-sum-helper
           (- curr-sum (/ (* ( expt 2 (+ curr-n 1)) (expt x curr-n)) (* prev-den curr-den)))
           (+ curr-n 1)
           (* prev-den curr-den)
           (+ curr-den 2))]
         [else
          (calc-series-sum-helper
           (+ curr-sum (/ (* ( expt 2 (+ curr-n 1)) (expt x curr-n)) (* prev-den curr-den)))
           (+ curr-n 1)
           (* prev-den curr-den)
           (+ curr-den 2))]))
           (calc-series-sum-helper -2 1 1 3))

(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285
