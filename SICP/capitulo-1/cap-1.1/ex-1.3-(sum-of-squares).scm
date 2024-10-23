; -------------------------------- Exercise 1.3 ----------------------------------
; Define a procedure that takes three numbers as arguments and returns the sum of
; the squares of the two larger numbers.

#lang sicp

; Función para calcular la suma de los cuadrados de dos números
(define (sum-cuadrados x y)
  (+ (* x x) (* y y)))

; Función principal que determina los dos números mayores y calcula la suma de
; sus cuadrados
(define (answer a b c)
  (cond ((and (> a b) (> b c)) (sum-cuadrados a b))
        ((and (> a c) (> c b)) (sum-cuadrados a c))
        ((and (> b a) (> a c)) (sum-cuadrados b a))
        ((and (> b c) (> c a)) (sum-cuadrados b c))
        ((and (> c a) (> a b)) (sum-cuadrados c a))
        ((and (> c b) (> b a)) (sum-cuadrados c b))))
