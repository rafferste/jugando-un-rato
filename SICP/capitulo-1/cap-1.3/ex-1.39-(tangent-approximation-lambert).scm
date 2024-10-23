#lang sicp
; ------------------------------- Exercise 1.39 --------------------------------   
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:

; tan(x) = x/(1-x^2/(3-x^2/(5-...)))

; where x is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert's formula. K specifies
; the number of terms to compute, as in exercise 1.37.

(define (tan-cf x k)
  (define (square n) (* n n))
  (define (recursiv d)
    (if (> d k)
        1.0
        (- d (/ (square x) (recursiv (+ d 2))))))
  
  (/ x (recursiv 1.0)))

