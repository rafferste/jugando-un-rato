#lang sicp
; -------------------------------- Exercise 1.2 ----------------------------------
; Translate the following expression into prefix form.

; (5+4+(2-(3-(6+4/5))))(3*(6-2)*(2-7))

; Respuesta:
; En formato prefijo:
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;
; pretty-printing
(/ (+ 5 4
      (- 2 (- 3
              (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; La forma prefija se lee de adentro hacia afuera, respetando la jerarquía
; de las operaciones según los paréntesis.
