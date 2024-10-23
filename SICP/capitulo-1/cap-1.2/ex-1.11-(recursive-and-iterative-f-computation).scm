#lang sicp
; -------------------------------- Exercise 1.11 ---------------------------------                     
; A function f is defined by the rule that f(n) = n if n<3 and 
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes 
; f by means of a recursive process. Write a procedure that computes f by means of 
; an iterative process.

; (Recursivo)
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; (Iterativo)
(define (f-iter n)
  (define (iter n cont fn-3 fn-2 fn-1)
    (if (> cont n)
        fn-1
        (iter n
              (inc cont)
              fn-2
              fn-1
              (+ fn-1 (* 2 fn-2) (* 3 fn-3)))))
   (if (< n 3)
       n
       (iter n 3 0 1 2)))