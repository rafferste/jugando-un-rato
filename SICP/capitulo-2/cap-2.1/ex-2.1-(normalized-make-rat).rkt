#lang sicp
; ------------------------------- Exercise 2.1 ---------------------------------   
; Define a better version of make-rat that handles both positive and negative
; arguments. Make-rat should normalize the sign so that if the rational number
; is positive, both the numerator and denominator are positive, and if the
; rational number is negative, only the numerator is negative.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (cond ((and (< n 0) (< d 0)) (cons (abs (/ n g)) (abs (/ d g))))
          ((< d 0) (cons (- (/ n g)) (abs (/ d g))))
          (else (cons (/ n g) (/ d g))))))
  
   