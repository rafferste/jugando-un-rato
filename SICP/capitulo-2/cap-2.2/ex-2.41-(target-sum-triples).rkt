#lang sicp
; ------------------------------- Exercise 2.41 --------------------------------
; Write a procedure to find all ordered triples of distinct positive integers i,
; j, and k less than or equal to a given integer n that sum to a given integer s

; (define (iqual-sum-tripeles n s))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map (lambda (k) (list i j k))
             (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (unique-sum-triples n s)
  (define (iqual-sum? triple)
    (= (accumulate + 0 triple) s))
  (filter iqual-sum? (unique-triples n)))
      
; Codigo viejo ----------------------------------------------------------------
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
  
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))