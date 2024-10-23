#lang sicp
; ------------------------------- Exercise 1.38 --------------------------------   
; In 1737, the Swiss mathematician Leonhard Euler published a memoir De
; Fractionibus Continuis, which included a continued fraction expansion for
; e - 2, where e is the base of the natural logarithms. In this fraction, the Ni
; are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
; Write a program that uses your cont-frac procedure from exercise 1.37 to
; approximate e, based on Euler's expansion.

(define (cont-frac n d k)
  (define (recursiv i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recursiv (+ i 1))))))
  (recursiv 1))

 ; Funcion definida para que devuelva un valor de D segun corresponda i
(define (sucesion i)
  (if (= (remainder (- i 2) 3) 0) 
      ; Con esta cuenta se puede obtener los multimplos de 2 segun avanza i
      (* (+ (/ (- i 2) 
               3)
            1)
         2.0)
      1.0))

; Calculando e con cont-frac + 2
(+ (cont-frac (lambda (i) 1.0) sucesion 10)
   2)