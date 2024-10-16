#lang sicp
; ------------------------------- Exercise 2.5 ---------------------------------   
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer that
; is the product 2a 3b. Give the corresponding definitions of the procedures
; cons, car, and cdr.

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (define (divisible-2? n)
    (= (remainder n 2)
       0))
  (define (iter num cont)
    (if (divisible-2? num)
        (iter (/ num 2) (+ cont 1))
        cont))
  (iter z 0))

(define (cdr z)
  (define (divisible-3? n)
    (= (remainder n 3)
       0))
  (define (iter num cont)
    (if (divisible-3? num)
        (iter (/ num 3) (+ cont 1))
        cont))
  (iter z 0))

(define (make-pair a b) (cons a b))

; Pruebas ------------------
(define t (make-pair 34 81))
(car t)
(cdr t)