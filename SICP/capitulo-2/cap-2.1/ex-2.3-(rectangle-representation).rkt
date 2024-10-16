#lang sicp
; ------------------------------- Exercise 2.3 ---------------------------------   
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of exercise 2.2.) In terms of your constructors and selectors, create
; procedures that compute the perimeter and the area of a given rectangle. Now
; implement a different representation for rectangles. Can you design your
; system with suitable abstraction barriers, so that the same perimeter and area
; procedures will work using either representation?

; Representacion 1
; (Utilizo la base y la altura)
; (define (make-rectangle b h) (cons b h))

; Representacion 2
; (Utilizo 3 puntos para crear 2 segmentos, los cuales deben ser perpendiculares)
; El punto b es compartido por ambos segmentos
(define (make-rectangle a b c)
  (define (perpendicular? s1 s2)
    (= (+ (* (x-point (end-segment s1)) (x-point (end-segment s2)))
          (* (y-point (end-segment s1)) (y-point (end-segment s2))))
       0))
  (let ((seg1 (make-segment b a))
        (seg2 (make-segment b c)))
    (if (perpendicular? seg1 seg2)
        (cons (length seg1)(length seg2))
        (error "Los puntos no forman dos segmentos perpendiculares"))))

; Para representar el rectangulo necesito la longitud de los segmentos
(define (length AB)
  (sqrt (+ (expt (- (x-point (end-segment AB))
                 (x-point (start-segment AB)))
                 2)
           (expt (- (y-point (end-segment AB))
                 (y-point (start-segment AB)))
                 2))))

; Selectores --------------
(define (base r) (car r))
(define (height r) (cdr r))

(define (perimeter r)
  (+ (* 2 (base r))
     (* 2 (height r))))

(define (area r)
  (* (base r) (height r)))

; Codigo del ejercicio 2.2 -----------
(define (make-segment A B) (cons A B))
(define (start-segment AB) (car AB))
(define (end-segment AB) (cdr AB))
(define (make-point x y) (cons x y))
(define (x-point A) (car A))
(define (y-point A) (cdr A))

; Pruebas ---------------------------
; Representacion 1 ------------------
; (define r1 (make-rectangle 3 2))

; Representacion 2 ------------------
(define P1 (make-point 0 2))
(define P2 (make-point 0 0))
(define P3 (make-point 3 0))
(define r1 (make-rectangle P1 P2 P3))

(perimeter r1)
(area r1)