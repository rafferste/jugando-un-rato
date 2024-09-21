#lang sicp
; -------------------------------- Exercise 1.32 ---------------------------------   
; a. Show that sum and product (exercise 1.31) are both special cases of a still
; more general notion called accumulate that combines a collection of terms, using
; some general accumulation function:

; (accumulate combiner null-value term a next b)

; Accumulate takes as arguments the same term and range specifications as sum and
; product, together with a combiner procedure (of two arguments) that specifies how
; the current term is to be combined with the accumulation of the preceding terms
; and a null-value that specifies what base value to use when the terms run out.
; Write accumulate and show how sum and product can both be defined as simple calls
; to accumulate.

; b. If your accumulate procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write one
; that generates a recursive process.

; proceso recursivo --------------------------------------------------------
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b) )))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; Pruebas pa' ver si todo va bien
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10) ; == 3025

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10) ; == 55

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))
(factorial 5) ; == 120

; proceso iterativo ------------------------------------------------------
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

; Pruebas pa' ver si todo va bien
(define (sum-cubes-iter a b)
  (sum-iter cube a inc b))
(sum-cubes-iter 1 10) ; == 3025


(define (sum-integers-iter a b)
  (sum-iter identity a inc b))
(sum-integers-iter 1 10) ; == 55

(define (factorial-iter n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product-iter identity 1 inc n))
(factorial-iter 5) ; == 120
