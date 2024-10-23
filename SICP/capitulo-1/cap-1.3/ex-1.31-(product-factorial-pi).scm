#lang sicp
; -------------------------------- Exercise 1.31 ---------------------------------   
; a.  The sum procedure is only the simplest of a vast number of similar
; abstractions that can be captured as higher-order procedures.51 Write an
; analogous procedure called product that returns the product of the values of a
; function at points over a given range. Show how to define factorial in terms of
; product. Also use product to compute approximations to  using the formula52

; pi/4 = (2*4*4*6*6*8*...)/(3*3*5*5*7*7*...)

; b.  If your product procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write one
; that generates a recursive process.

; Definición de la función "product" que genera un proceso recursivo
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Definición de la función "product" que genera un proceso iterativo
(define (product-iter term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

; Definición del factorial usando la función "product"
(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))

; Aproximación de π usando la fórmula dada
(define (pi-v1 n)
  (define (identity x) x)
  (define (inc x) (+ x 2))
  (define (square x) (* x x))
  (if (even? n) ; Si 'n' es par, se sigue una fórmula diferente que si es impar.
      (/ (* 2
            4.0
            (square (product identity 4 inc (- n 1))) ; Numerador
            n)
          (square (product identity 3 inc n))) ; Denominador
      (/ (* 2
            4.0
            (square (product identity 4 inc n))) ; Numerador
         (* (square (product identity 3 inc (- n 1))) ; Denominador
            n))))

(display "pi-v1(100) ---> ")
(pi-v1 100)
(display "pi-v1(101) ---> ")
(pi-v1 101)
(display "pi-v1(1000) --> ")
(pi-v1 1000)
(newline)

; (pi-v1 1000) ---> +inf.0
; Por la forma en que está definido el procedimiento anterior, cuando quiero
; calcular pi con 1000 numeros, se desborda la capacidad del lenguaje y me
; devuelve que el valor es infinito positivo, esto sucede porque primero calculo
; el cuadrado de la multiplicacion del divisor para luego dividirlo. Lo que
; implementare a continucacion será una modificacion para calcular las divisiones
; parciales y luego multiplicarlas entre sí, de esa forma evito el desbordamiento

; Modificación de pi-v1 para evitar el desbordamiento
(define (pi-v2 n)
  (define (divide x) (/ x (- x 1))) ; Define una función que divide x por (x - 1)
  (define (inc x) (+ x 2))
  (define (square x) (* x x))
  (if (even? n) ; Si 'n' es par, sigue la fórmula específica.
      (* 2
         4.0
         (square (product divide 4 inc n)) ; Numerador: cálculo de producto parcial.
         (/ 1 (- n 1))) ; Cálculo de división parcial.
      (* 2
         4.0
         (square (product divide 4 inc (+ n 1))) ; Numerador: cálculo de producto parcial.
         (/ 1 (+ n 2))))) ; Cálculo de división parcial.

(display "pi-v2(100) ---> ")
(pi-v2 100)
(display "pi-v2(101) ---> ")
(pi-v2 101)
(display "pi-v2(1000) --> ")
(pi-v2 1000)
(display "pi-v2(10000) --> ")
(pi-v2 10000)


