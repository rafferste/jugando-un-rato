#lang sicp
;-------------------------------- Exercise 1.24 ---------------------------------   
;Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the
;Fermat method), and test each of the 12 primes you found in that exercise. Since
;the Fermat test has (log n) growth, how would you expect the time to test primes
;near 1,000,000 to compare with the time needed to test primes near 1000? Do your
;data bear this out? Can you explain any discrepancy you find?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 3) ;; Modifiqué acá para que verificara con Fermat y con 3 numeros
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; ------------------------------------------------------------------------
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x) (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))
; ------------------------------------------------------------------------
; n representa el numero a partir del cual se buscan los primos
(define (three-prime n)
  (define (iter n cont)
    (cond ((= cont 0) (newline))
          ((divides? n 2) (iter (+ n 1) cont))
          ((prime? n) (begin (timed-prime-test n) (iter (+ n 1) (- cont 1))))
          (else (iter (+ n 1) cont))))
  (iter n 3))

; -----------------------------------------------------------------------
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

;(three-prime 1000)
;(three-prime 10000)
;(three-prime 100000)
;(three-prime 1000000)

;Se  puede observar un leve cambio de tiempo entre primos cercanos a
;1.000 con respectos a los de 1.000.000, era de esperar este comportamiento
;gracias al crecimiento de log(n), hace magia laverdad, mientras que en los
;otros metodos el proceso tardaba practicamente 10 veces mas, en este solo
;tarda un 0.3 mas, vendría siendo 30 veces mas eficiente para este tamaño
;de numeros, claro que a mayor n, mayor va a ser la diferencia de eficiencia