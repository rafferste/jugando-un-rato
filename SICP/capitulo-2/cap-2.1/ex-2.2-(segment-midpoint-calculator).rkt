#lang sicp
; ------------------------------- Exercise 2.2 ---------------------------------   
; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and an ending point. Define
; a constructor make-segment and selectors start-segment and end-segment that
; define the representation of segments in terms of points. Furthermore, a point
; can be represented as a pair of numbers: the x coordinate and the y
; coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation. Finally, using your
; selectors and constructors, define a procedure midpoint-segment that takes a
; line segment as argument and returns its midpoint (the point whose coordinates
; are the average of the coordinates of the endpoints). To try your procedures,
; you'll need a way to print points:
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display " ; ")
  (display (y-point p))
  (display ")"))

(define (make-segment A B) (cons A B))
(define (start-segment AB) (car AB))
(define (end-segment AB) (cdr AB))  

(define (make-point x y) (cons x y))
(define (x-point A) (car A))
(define (y-point A) (cdr A))

(define (midpoint-segment AB)
  (make-point (average (x-point (start-segment AB)) (x-point (end-segment AB)))
              (average (y-point (start-segment AB)) (y-point (end-segment AB)))))

(define (average x y)
  (/ (+ x y)
     2.0))

; Pruebas--------------------------
(define P1 (make-point 1 2))
(define P2 (make-point 5 2))
(define s1 (make-segment P1 P2))
(print-point (midpoint-segment s1))