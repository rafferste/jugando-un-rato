#lang sicp
;-------------------------------- Exercise 1.28 ---------------------------------   
;One variant of the Fermat test that cannot be fooled is called the Miller-Rabin
;test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's
;Little Theorem, which states that if n is a prime number and a is any positive
;integer less than n, then a raised to the (n - 1)st power is congruent to 1
;modulo n. To test the primality of a number n by the Miller-Rabin test, we pick
;a random number a<n and raise a to the (n - 1)st power modulo n using the expmod
;procedure. However, whenever we perform the squaring step in expmod, we check to
;see if we have discovered a ``nontrivial square root of 1 modulo n,'' that is, a
;number not equal to 1 or n - 1 whose square is equal to 1 modulo n. It is
;possible to prove that if such a nontrivial square root of 1 exists, then n is
;not prime. It is also possible to prove that if n is an odd number that is not
;prime, then, for at least half the numbers a<n, computing an-1 in this way will
;reveal a nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test
;cannot be fooled.) Modify the expmod procedure to signal if it discovers a
;nontrivial square root of 1, and use this to implement the Miller-Rabin test with
;a procedure analogous to fermat-test. Check your procedure by testing various
;known primes and non-primes. Hint: One convenient way to make expmod signal is to
;have it return 0.

; Prueba de Fermat para determinar si un número es probablemente primo
(define (fermat-test n)
  (define (try-it a)
    ; Verifica si a^(n-1) ≡ 1 (mod n), lo cual es un indicio de que n podría ser primo
    (= (expmod a (- n 1) n) 1))
  ; Selecciona un número aleatorio a entre 1 y n-1 y realiza la prueba de Fermat
  (try-it (+ 1 (random (- n 1)))))

; Procedimiento que realiza múltiples pruebas de Fermat para determinar si n es primo
(define (fast-prime? n times)
  (cond ((< times 0) true)  ; Si se han hecho suficientes pruebas, n es probablemente primo
        ; Si pasa la prueba de Fermat, continúa con una prueba menos
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))  ; Si no pasa la prueba de Fermat, n no es primo

; Calcula (base^exp) mod m de manera eficiente utilizando exponenciación modular
(define (expmod base exp m)
  (cond ((= exp 0) 1)  ; Cualquier número elevado a 0 es 1
        ((even? exp)
         ; Si exp es par, calcula (base^(exp/2))^2 mod m
         (remainder (square-tivial? (expmod base (/ exp 2) m)
                                    m)
                    m))
        ; Si exp es impar, calcula base * (base^(exp-1)) mod m
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Verifica si x es una raíz cuadrada no trivial de 1 mod n
(define (square-tivial? x n)
  ; Si x^2 ≡ 1 mod n y x no es 1 ni n-1, entonces n no es primo (retorna 0)
  (if (and (= (square x) (remainder 1 n)) (not (= x 1)) (not (= x (- n 1))))
      0
      (square x)))  ; Si no es una raíz cuadrada no trivial, retorna x^2

; Calcula el cuadrado de un número
(define (square x) (* x x))

; Prueba principal que verifica si n es probablemente primo realizando n/2 pruebas de Fermat
(define (prime? n)
  (fast-prime? n (/ n 2.0)))

