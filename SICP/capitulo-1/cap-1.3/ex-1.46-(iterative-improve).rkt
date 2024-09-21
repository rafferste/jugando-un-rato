#lang sicp
; ------------------------------- Exercise 1.46 --------------------------------   
; Several of the numerical methods described in this chapter are instances of an
; extremely general computational strategy known as iterative improvement.
; Iterative improvement says that, to compute something, we start with an
; initial guess for the answer, test if the guess is good enough, and otherwise
; improve the guess and continue the process using the improved guess as the new
; guess. Write a procedure iterative-improve that takes two procedures as
; arguments: a method for telling whether a guess is good enough and a method
; for improving a guess. Iterative-improve should return as its value a
; procedure that takes a guess as argument and keeps improving the guess until
; it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the
; fixed-point procedure of section 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

; Nueva procedimiento para calcular la raiz cuadrada ---------------------------
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2.0))
  
  ((iterative-improve good-enough? improve) 1.0))

; Nuevo procedimiento para calcular fixed-point --------------------------------
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? guess)
    (let ((next (f guess)))
      (< (abs (- guess next)) tolerance)))
  (define improve f)
  
  ((iterative-improve good-enough? improve) first-guess))

; Pruebas ----------------------------------------------------------------------
(sqrt 16)
(sqrt 81)
(fixed-point cos 1.0) ; ------------------------------------> 0.7390822985224023
(fixed-point (lambda (y) (+ (sin y) (cos y))); -------------> 1.2587315962971173
             1.0) 





