#lang sicp
; ------------------------------- Exercise 1.45 --------------------------------   
; We saw in section 1.3.3 that attempting to compute square roots by naively
; finding a fixed point of y->x/y does not converge, and that this can be fixed
; by average damping. The same method works for finding cube roots as fixed
; points of the average-damped y->x/y^2. Unfortunately, the process does not
; work for fourth roots -- a single average damp is not enough to make a
; fixed-point search for y->x/y^3 converge. On the other hand, if we average
; damp twice (i.e., use the average damp of the average damp of y->x/y<^3) the
; fixed-point search does converge. Do some experiments to determine how many
; average damps are required to compute nth roots as a fixed-point search based
; upon repeated average damping of y  x/yn-1. Use this to implement a simple
; procedure for computing nth roots using fixed-point, average-damp, and the
; repeatedrocedure of exercise 1.43. Assume that any arithmetic operations you
; need are available as primitives.

; Pruebas para encontrar el patron ---------------------------------------------
; (Todas convergen con el minimo de repeticiones) ------------------------------
(define (sqrt x)
  (fixed-point ((repeated average-damp 1) (lambda (y) (/ x (expt y 1))))
               1.0))
(define (cube-root x)
  (fixed-point ((repeated average-damp 1) (lambda (y) (/ x (expt y 2))))
               1.0))
(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 3))))
               1.0))
(define (fifth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 4)))) 
               1.0))
(define (sixth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 5))))
               1.0))
(define (seventh-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 6))))
               1.0))
(define (eighth-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 7))))
               1.0))
(define (ninth-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 8))))
               1.0))
(define (tenth-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 9))))
               1.0))
(define (eleventh-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 10))))
               1.0))
(define (twelfth-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 11))))
               1.0))
(define (thirteenth-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 12))))
               1.0))
(define (14-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 13))))
               1.0))
(define (15-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 14))))
               1.0))
(define (16-root x)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (expt y 15))))
               1.0))
; Código solucion --------------------------------------------------------------
(define (root x n)
  (define (cant i)
      (cond ((= (expt 2 i) n) i)
            ((> (expt 2 i) n) (- i 1))
            (else (cant (+ i 1)))))
  
  (fixed-point ((repeated average-damp (cant 1)) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

; Codigo viejo -----------------------------------------------------------------
; repeated (ej. 1.43) ----------------------------------------------------------
(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (lambda (x)
    (if (< n 2)
        (f x)
        ((compose (repeated f (- n 1)) f)
                 x))))

; fixed-point ------------------------------------------------------------------
(define tolerance 0.0000000000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next) 
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; average-damp -----------------------------------------------------------------
(define (average a b) (/ (+ a b) 2.0))
(define (average-damp f)
  (lambda (y) (average y (f y))))
