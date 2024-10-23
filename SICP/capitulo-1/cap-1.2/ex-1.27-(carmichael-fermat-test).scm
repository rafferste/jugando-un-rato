#lang sicp
;-------------------------------- Exercise 1.25 ---------------------------------  
;Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the
;Fermat test. That is, write a procedure that takes an integer n and tests
;whether an is congruent to a modulo n for every a<n, and try your procedure on
;the given Carmichael numbers.


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x) (* x x))

(fast-prime? 561 561)
(fast-prime? 1105 1105)
(fast-prime? 1729 1729)
(fast-prime? 2465 2465)
(fast-prime? 2821 2821)
(fast-prime? 6601 6601)