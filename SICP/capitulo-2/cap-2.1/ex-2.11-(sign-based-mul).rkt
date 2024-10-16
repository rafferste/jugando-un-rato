#lang sicp
; ------------------------------- Exercise 2.11 --------------------------------   
; In passing, Ben also cryptically comments: ``By testing the signs of the
; endpoints of the intervals, it is possible to break mul-interval into nine
; cases, only one of which requires more than two multiplications.'' Rewrite
; this procedure using Ben's suggestion.

(define (mul-interval x y)
  (if (>= (lower-bound x) 0)
      ; (+;+)*(?;?)
      (if (>= (lower-bound y) 0)
          ; (+;+)*(+;+)
          (make-interval (* (lower-bound x) (lower-bound y)) 
                         (* (upper-bound x) (upper-bound y)))
          ; (+;+)*(-;?)
          (if (>= (upper-bound y) 0)
              ; (+;+)*(-;+)
              (make-interval (* (upper-bound x) (lower-bound y)) 
                             (* (upper-bound x) (upper-bound y)))
              ; (+;+)*(-;-)
              (make-interval (* (upper-bound x) (lower-bound y)) 
                             (* (lower-bound x) (upper-bound y)))))
      ; (-;?)*(?;?)
      (if (>= (upper-bound x) 0)
          ; (-;+)*(?;?)
          (if (>= (lower-bound y) 0)
              ; (-;+)*(+;+)
              (make-interval (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (upper-bound y)))
              ; (-;+)*(-;?)
              (if (>= (upper-bound y) 0)
                  ; (-;+)*(-;+)
                  (make-interval (min (* (lower-bound x) (upper-bound y))
                                      (* (upper-bound x) (lower-bound y)))
                                 (max (* (upper-bound x) (upper-bound y))
                                      (* (lower-bound x) (lower-bound y))))
                  ; (-;+)*(-;-)
                  (make-interval (* (upper-bound x) (lower-bound y)) 
                                 (* (lower-bound x) (lower-bound y)))))
          ; (-;-)*(?;?)
          (if (>= (lower-bound y) 0)
              ; (-;-)*(+;+)
              (make-interval (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (lower-bound y)))
              ; (-;-)*(-;?)
              (if (>= (upper-bound y) 0)
                  ; (-;-)*(-;+)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y)))
                  ; (-;-)*(-;-)
                  (make-interval (* (upper-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y))))))))
             

; Codigo viejo ----------------------------------
(define (make-interval a b) (cons a b))
(define (upper-bound z) (cdr z))
(define (lower-bound z) (car z))

; Casos de prueba
(define interval-pos-pos (make-interval 2 4))    ; (+; +)
(define interval-neg-neg (make-interval -4 -2))  ; (-; -)
(define interval-neg-pos (make-interval -3 2))   ; (-; +)
(define interval-zero-pos (make-interval 0 3))   ; (0; +)
(define interval-neg-zero (make-interval -3 0))  ; (-; 0)

; Pruebas con diferentes combinaciones de intervalos
(display "Pruebas: \n")
(display "Positivo * Positivo\n")
(display (mul-interval interval-pos-pos interval-pos-pos)) ; (+; +) * (+; +)
(newline)

(display "Positivo * Negativo\n")
(display (mul-interval interval-pos-pos interval-neg-neg)) ; (+; +) * (-; -)
(newline)

(display "Positivo * Mixto (-; +)\n")
(display (mul-interval interval-pos-pos interval-neg-pos)) ; (+; +) * (-; +)
(newline)

(display "Negativo * Negativo\n")
(display (mul-interval interval-neg-neg interval-neg-neg)) ; (-; -) * (-; -)
(newline)

(display "Mixto * Mixto\n")
(display (mul-interval interval-neg-pos interval-neg-pos)) ; (-; +) * (-; +)
(newline)

(display "Positivo * (0; +)\n")
(display (mul-interval interval-pos-pos interval-zero-pos)) ; (+; +) * (0; +)
(newline)

(display "Negativo * (-; 0)\n")
(display (mul-interval interval-neg-neg interval-neg-zero)) ; (-; -) * (-; 0)
(newline)

(display "Mixto * (0; +)\n")
(display (mul-interval interval-neg-pos interval-zero-pos)) ; (-; +) * (0; +)
(newline)

(display "Mixto * (-; 0)\n")
(display (mul-interval interval-neg-pos interval-neg-zero)) ; (-; +) * (-; 0)
(newline)