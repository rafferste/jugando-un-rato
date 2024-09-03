#lang sicp
;-------------------------------- Exercise 1.18 ---------------------------------   
;Using the results of exercises 1.16 and 1.17, devise a procedure that generates 
;an iterative process for multiplying two integers in terms of adding, doubling, 
;and halving and uses a logarithmic number of steps.

(define (iter-mult a b)
  (define (iter a b result)
    (cond ((= b 1) result)
          ((even? b) (iter a (halve b) (+ result(double a))))
          ((> b 1) (iter a (- b 1) (+ result a)))
          (else result)))
  (if (= b 0)
      1
      (iter a b 0)))

(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))