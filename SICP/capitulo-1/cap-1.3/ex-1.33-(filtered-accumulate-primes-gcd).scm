#lang sicp
; ------------------------------- Exercise 1.33 --------------------------------   
; You can obtain an even more general version of accumulate (exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:

(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filter-accumulate filter
                                                 combiner
                                                 null-value
                                                 term
                                                 (next a)
                                                 next
                                                 b)))
        (else (filter-accumulate filter
                                 combiner
                                 null-value
                                 term
                                 (next a)
                                 next
                                 b))))


; a. the sum of the squares of the prime numbers in the interval a to b
; (assuming that you have a prime? predicate already written)

; Procedimiento básico para saber si un numero es primo
; -----------------------------------------------------
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (square x) (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
; -----------------------------------------------------

(define (sum-square-prime a b)
  (filter-accumulate prime?
                     +
                     0
                     square
                     a
                     inc
                     b))

(sum-square-prime 1 3) ; 1*1 + 2*2 + 3*3 == 14
(sum-square-prime 3 10) ; 3*3 + 5*5 + 7*7 == 83
                     
; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

; Procedimiento básico para encontrar el Greatest Common Divisor (GCD)
; --------------------------------------------------------------------
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; --------------------------------------------------------------------

(define (product-coprime n)
  (define (test x) (= (gcd x n) 1)) ; Funcion para seleccionar los números relativamente primos a n.
  (define (identity x) x)
  (filter-accumulate test
                     *
                     1
                     identity
                     1
                     inc
                     n))

(product-coprime 3) ; 1 * 2 == 2
(product-coprime 8) ; 1 * 3 * 5 * 7 == 105


