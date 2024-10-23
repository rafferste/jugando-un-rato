#lang sicp
; -------------------------------- Exercise 1.22 ---------------------------------   
; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson's Rule, the integral of a function f
; between a and b is approximated as

; (h/3)*[y0 +4y1+ 2y2 + 4y3 + 2y4 + ... + 2y(n-2) + 4y(n-1) + yn]

; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n
; increases the accuracy of the approximation.) Define a procedure that takes as ;
; arguments f, a, b, and n and returns the value of the integral, computed using
; Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100
; and n = 1000), and compare the results to those of the integral procedure shown
; above.

; Función que calcula una integral usando la regla de Simpson
(define (integral-s f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))

  ; Función que suma los términos según la regla de Simpson
  (define (sum term k next n)
    (define y (+ a (* k h))) ; Calcula el valor de y en la posición k
    (cond ((> k n) 0)         ; Si k es mayor que n, devuelve 0 (fin de la recursión)
          ((or (= k 0) (= k n)) ; Si k es 0 o n, suma el término sin multiplicador
           (+ (term y)
              (sum term (next k) next n)))
          ((even? k) (+ (* 2 (term y)) ; Si k es par, multiplica el término por 2
                        (sum term (next k) next n)))
          (else (+ (* 4 (term y)) ; Si k es impar, multiplica el término por 4
                     (sum term (next k) next n)))))
    
  ; Multiplica la suma obtenida por h/3 y devuelve el resultado final
  (* (/ h 3.0) (sum f 0 inc n)))

; -----------------------------------------------------------------------------------

; Función que calcula una integral aproximada sumando áreas de rectángulos
(define (integral f a b dx)
  ; add-dx desplaza x por el ancho dx
  (define (add-dx x) (+ x dx))
  ; Suma de las áreas de los rectángulos aproximando la integral
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Función general para sumar términos de una función f
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) ; Suma el término actual con la suma de los siguientes
         (sum term (next a) next b))))

(define (cube x) (* x x x))


(display "Integral aprox (0.01) --> ")
(integral cube 0 1 0.01)

(display "Simpson's Rule (100) ---> ")
(integral-s cube 0 1 100)

(display "Integral aprox (0.001) -> ")
(integral cube 0 1 0.001)

(display "Simpson's Rule (1000) --> ")
(integral-s cube 0 1 1000)
