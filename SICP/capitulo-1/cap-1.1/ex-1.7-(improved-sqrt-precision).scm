; -------------------------------- Exercise 1.7 ----------------------------------
; The good-enough? test used in computing square roots will not be very effective 
; for finding the square roots of very small numbers. Also, in real computers, 
; arithmetic operations are almost always performed with limited precision. This 
; makes our test inadequate for very large numbers. Explain these statements, with 
; examples showing how the test fails for small and large numbers. An alternative 
; strategy for implementing good-enough? is to watch how guess changes from one 
; iteration to the next and to stop when the change is a very small fraction of 
; the guess. Design a square-root procedure that uses this kind of end test. Does 
; this work better for small and large numbers?

;Con la impletementacion de good-enought vieja:
;A partir de la raiz cuadrada de 0.0001, la respuesta siempre sera muy impresiza
;Cuando trato de encontrar la raiz cuadrada de 9999999999999999999999999999999999
;en adelante el resultado se redonde a notacion cientifica, ej: 1e+17

#lang sicp

(define (good-enough? guess x)
  ; El test compara la diferencia absoluta entre la mejora
  ; del guess actual y el guess anterior con una fracción del
  ; número original.
  (< (abs (- (improve guess x) guess)) (* x 0.001)))

(define (sqrt-iter guess x)
  ; Itera hasta que el guess sea suficientemente preciso.
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  ; Mejora el guess calculando el promedio de guess y x/guess.
  (average guess (/ x guess)))

(define (average x y)
  ; Calcula el promedio de dos números.
  (/ (+ x y) 2))

(define (sqrt x)
  ; Calcula la raíz cuadrada iniciando el guess en 1.0.
  (sqrt-iter 1.0 x))

;Si funciona mejor, ahora acepta un tamaños muchisimo mas pequeño o mas grande 
;de numero, y la respuesta no la redondea a 1 como pasaba antes
