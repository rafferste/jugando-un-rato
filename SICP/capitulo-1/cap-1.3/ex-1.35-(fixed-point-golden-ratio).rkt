#lang sicp
; ------------------------------- Exercise 1.35 --------------------------------   
; Show that the golden ratio (phi) (section 1.2.2) is a fixed point of the
; transformation x -> 1 + 1/x, and use this fact to compute (phi) by means of
; the fixed-point procedure.

; Para demostrar que (phi) es un punto fijo de x -> 1 + 1/x. se debe cumplir que

; (phi) = 1 + 1/(phi) --> (phi) - 1/(phi) = 1 --> ((phi)^2 - 1)/(phi) = 1
; (phi)^2 - 1 = (phi) --> (phi)^2 - (phi) - 1 = 0
; |--> (phi) = (1 + sqrt(5))/2 = 1.61803...
; |--> (phi) = (1 - sqrt(5))/2 = -0.61803...

; Que son los valores de phi correctos

(define tolerance 0.00000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) -100.0)
