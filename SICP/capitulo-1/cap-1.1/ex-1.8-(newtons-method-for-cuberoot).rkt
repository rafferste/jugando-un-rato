; -------------------------------- Exercise 1.8 ----------------------------------
; Newton's method for cube roots is based on the fact that if y is an 
; approximation to the cube root of x, then a better approximation is 
; given by the value (x/y + 2y) / 3.
;
; Implement a cube-root procedure analogous to the square-root procedure.

#lang sicp

; Función para mejorar la aproximación de la raíz cúbica
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

; Función que determina si la aproximación es suficientemente buena
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (* x 0.001)))

; Iteración para encontrar la raíz cúbica
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

; Función principal para calcular la raíz cúbica
(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (square x) (* x x))

; Ejemplo de uso:
(cube-root 27) ; Debería devolver 3
(cube-root 8)  ; Debería devolver 2
