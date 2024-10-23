#lang sicp
; ------------------------------- Exercise 1.40 --------------------------------   
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form

; (newtons-method (cubic a b c) 1)

; to approximate zeros of the cubic x3 + ax2 + bx + c.

; Codigo del libro ---------------------------------------------
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cub x) (* x x x))
(define (square x) (* x x))

; Codigo nuevo ------------------------------------------------
(define (cubic a b c)
  (lambda (x)
    (+ (cub x) (* a (square x)) (* b x) c)))
