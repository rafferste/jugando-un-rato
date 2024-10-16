#lang sicp
; ------------------------------- Exercise 2.14 --------------------------------   
; Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions. Make some intervals A and B, and use them
; in computing the expressions A/A and A/B. You will get the most insight by
; using intervals whose width is a small percentage of the center value. Examine
; the results of the computation in center-percent form (see exercise 2.12).

; Codigo ex-2.12 ---------------------------------------------------------------
(define (make-center-percent c p)
  (let ((width (* c (/ p 100))))
    (make-interval (- c width) (+ c width))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (* (width i) 100)
     (center i)))

; Codigo del libro -------------------------------------------------------------
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (make-interval a b) (cons a b))
(define (upper-bound z) (cdr z))
(define (lower-bound z) (car z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Pruebas ----------------------------------------------------------------------
(define A (make-center-percent 5 0.1))
(define B (make-center-percent 25.3 0.1))

(par1 A A)
(- (upper-bound (par1 A A)) (lower-bound (par1 A A)))

(par2 A A)
(- (upper-bound (par2 A A)) (lower-bound (par2 A A)))

(newline)

(par1 A B)
(- (upper-bound (par1 A B)) (lower-bound (par1 A B)))

(par2 A B)
(- (upper-bound (par2 A B)) (lower-bound (par2 A B)))

; Se puede apreciar en los resultados que el ancho generado por par1 es bastante
; mayor que el generado por par2, aproximadamente 3 veces mas