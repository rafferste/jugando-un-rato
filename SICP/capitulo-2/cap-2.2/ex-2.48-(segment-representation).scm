#lang sicp
; ------------------------------- Exercise 2.48 --------------------------------
; A directed line segment in the plane can be represented as a pair of vectors
; -- the vector running from the origin to the start-point of the segment, and
; the vector running from the origin to the end-point of the segment. Use your
; vector representation from exercise 2.46 to define a representation for
; segments with a constructor make-segment and selectors start-segment and
; end-segment.

(define (make-segment start-vect end-vect)
  (cons start-vect (sub-vect end-vect start-vect)))

(define (start-segment s) (car s))

(define (end-segment s)
  (add-vect (car s) (cdr s)))

; Codigo viejo -----------------------------------------------------------------
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect a v)
  (make-vect (* a (xcor-vect v))
             (* a (ycor-vect v))))

; Código de prueba -------------------------------------------------------------
(define v1 (make-vect 1 3))
(define v2 (make-vect 4 8))
(define seg (make-segment v1 v2))

(start-segment seg) ; debería devolver v1
(end-segment seg)   ; debería devolver v2