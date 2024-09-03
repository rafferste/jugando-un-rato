; -------------------------------- Exercise 1.4 ----------------------------------
; Observe that our model of evaluation allows for combinations whose operators are
; compound expressions. Use this observation to describe the behavior of the 
; following procedure:

#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Ejemplo de uso:
(a-plus-abs-b 3 -5) ; Desglosado paso a paso:

; Paso 1: Se llama a la función con los argumentos 3 y -5.
; (a-plus-abs-b 3 -5)

; Paso 2: El operador es determinado por la condición if.
; ((if (> -5 0) + -) 3 -5)
; Como -5 no es mayor que 0, la condición es falsa.

; Paso 3: Dado que la condición es falsa, se selecciona el operador `-`.
; (- 3 -5)

; Paso 4: Se realiza la operación de resta.
; (- 3 -5) resulta en 8, ya que restar un número negativo es equivalente a sumarlo.

; Resultado final:
; 8
