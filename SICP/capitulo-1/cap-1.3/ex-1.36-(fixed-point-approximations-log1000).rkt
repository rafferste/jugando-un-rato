#lang sicp
; ------------------------------- Exercise 1.36 --------------------------------   
; Modify fixed-point so that it prints the sequence of approximations it
; generates, using the newline and display primitives shown in exercise 1.22.
; Then find a solution to xx = 1000 by finding a fixed point of
; x->log(1000)/log(x). (Use Scheme's primitive log procedure, which computes
; natural logarithms.) Compare the number of steps this takes with and without
; average damping. (Note that you cannot start fixed-point with a guess of 1, as
; this would cause division by log(1) = 0.)

; Defino una tolerancia para el procedimiento fixed-point
(define tolerance 0.000001)

; Defino el procedimiento fixed-point
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) ; Funcion para chequear si encontré el valor
    (< (abs (- v1 v2)) tolerance))
  (define (try guess) ; Funcion recursiva para aproximar el valor
    (let ((next (f guess))) ; Calculo la siguiente aproximacion
      (display next) ; Imprimo la aproximacion
      (newline)
      (if (close-enough? guess next) ; Chequeo si mi encontre el fixed-point
         next
         (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))

(display "-------COMUN-------")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) ; 29 pasos
             2)
(newline)
(display "----AMORTIGUADO----")
(newline)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) ; 9 pasos
             2)
