#lang sicp
; ------------------------------- Exercise 1.44 --------------------------------   
; The idea of smoothing a function is an important concept in signal processing.
; If f is a function and dx is some small number, then the smoothed version of f
; is the function whose value at a point x is the average of f(x - dx), f(x),
; and f(x + dx). Write a procedure smooth that takes as input a procedure that
; computes f and returns a procedure that computes the smoothed f. It is
; sometimes valuable to repeatedly smooth a function (that is, smooth the
; smoothed function, and so on) to obtained the n-fold smoothed function. Show
; how to generate the n-fold smoothed function of any given function using
; smooth and repeated from exercise 1.43.

; Suaviza una funcion
(define (smooth f)
  (define dx 0.001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

; Repite el suavizado de una funcion
(define (smooth-n f n)
  (repeated (smooth f) n))
          
; Codigo viejo ----------------------------------
(define (square x) (* x x))
; Compone dos funciones
(define (compose f g)
  (lambda (x) (f (g x))))
; Repite la aplicacion de una funcion
(define (repeated f n)
  (lambda (x)
    (if (< n 1)
        (f x)
        ((compose (repeated f (- n 2)) f)
                 x))))