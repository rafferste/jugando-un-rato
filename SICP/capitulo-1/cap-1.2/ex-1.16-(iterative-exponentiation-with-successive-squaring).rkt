#lang sicp
; -------------------------------- Exercise 1.16 ---------------------------------   
; Design a procedure that evolves an iterative exponentiation process that uses 
; successive squaring and uses a logarithmic number of steps, as does fast-expt. 
; (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the 
; exponent n and the base b, an additional state variable a, and define the state 
; transformation in such a way that the product a bn is unchanged from state to 
; state. At the beginning of the process a is taken to be 1, and the answer is 
; given by the value of a at the end of the process. In general, the technique of 
; defining an invariant quantity that remains unchanged from state to state is a 
; powerful way to think about the design of iterative algorithms.)

(define (iter-expt b n)
  (define (iter b n a)
     ; Caso base: cuando n es 0, devuelve el resultado acumulado a
    (cond ((= n 0) a)
          ; Cuando n es par, eleva al cuadrado la base y divide el exponente entre 2
          ((even? n) (iter b (/ n 2) (* (square b) a)))
          ; Cuando n es impar, multiplica el resultado acumulado por la base y decrementa el exponente
          ((> n 1) (iter b (- n 1) (* b a)))
          (else a)))
  (iter b n 1))

(define (square x) (* x x))