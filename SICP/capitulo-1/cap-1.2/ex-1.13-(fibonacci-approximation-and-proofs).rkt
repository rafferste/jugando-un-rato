#lang sicp
; -------------------------------- Exercise 1.13 ---------------------------------                     
; Prove that Fib(n) is the closest integer to z^n/5^(1/2), 
; where z = (1+5^/(1/2))/2. Hint: Let x = (1-5^(1/2))/2. Use induction and the 
; definition of the Fibonacci numbers (see section 1.2.2) to prove that 
; Fib(n) = (z^n - x^n)/5^(1/2).

; [A]Fibonacci aproximando al entero mas cercano 
(define (fib-aprox n)
  (define phi (/ (+ 1 (my-sqrt 5)) 2))
  (/ (pot+ phi n) (my-sqrt 5)))

; [B]Finonacci utilizando recurrencia F(n) = F(n-1) + F(n-2)
(define (fib-recur n)
  (define (iter cont fn fn-1)
    (if (= cont 1)
        fn
        (iter (- cont 1)
              (+ fn fn-1)
              fn)))
  (cond ((< n 1) 0)
        ((= n 1) 1)
        (else (iter n 1 0))))

; Demostracion que [A] es correcta comparandola con [B] 
(define (demost-A)
  (define (iter n)
    (cond ((>= (abs (- (fib-aprox n) (fib-recur n))) 0.5) n)
          ((= n 100) #t)
          (else (iter (+ n 1)))))
  (iter 0))
; Por temas de precision solo se cumple para los primeros 70 numeros

; [C]Fibonacci con fórmula de Binet
(define (fib-binet n)
  (define phi (/ (+ 1 (my-sqrt 5)) 2))
  (define psi (/ (- 1 (my-sqrt 5)) 2))
  (/ (- (pot+ phi n)
        (pot+ psi n))
     (my-sqrt 5)))

;Demostracion que [C] es correcta comparandola con [B] 
(define (demost-C)
  (define (iter n)
    (cond ((not(= (abs (- (fib-binet n) (fib-recur n))) 0)) n)
          ((= n 100) #t)
          (else (iter (+ n 1)))))
  (iter 0))
; Por temas de precision solo se cumple para los primeros 9 numeros

;----------------------------------------------------------------------------
(define (my-sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess x)
    (< (abs (- (improve guess x) guess)) (* x 0.00000000000001))) 
                                            ;^Limite de precision que es útil
  (define (square x)
    (* x x))

  (sqrt-iter 1.0 x))
;----------------------------------------------------------------------------
(define (pot+ x p)
  (define (iter y result)
    (if (= y 0)
        result
        (iter (- y 1) (* x result))))
  (if (< p 1)
      1
      (iter p 1)))