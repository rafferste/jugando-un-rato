#lang sicp
; ------------------------------- Exercise 2.32 --------------------------------   
; We can represent a set as a list of distinct elements, and we can represent
; the set of all subsets of the set as a list of lists. For example, if the set
; is (1 2 3), then the set of all subsets is:

; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; Complete the following definition of a procedure that generates the set of
; subsets of a set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 2 3))

; El procedimiento funciona porque por una parte en append tengo el primer
; rest que lo que hace es ir "acumulando" las respuestas de la recursividad
; es decir, primero vale (() (3)) luego (() (3) (2) (2 3)), etc. Mientras
; que el segundo rest sirve para utilizar cada uno de esos subconjuntos ya
; encontrados y convinarlo con el nuevo elemento que se encuentra en (car s)
; de esta forma append une la lista "vieja" que tenia de subconjuntos con
; la "nuevo" lista creada por todas las posibles convinaciones de ese nuevo
; elemnto que hay en (car s)