#lang sicp
; ------------------------------- Exercise 2.29 --------------------------------   
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or
; another binary mobile. We can represent a binary mobile using compound data by
; constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together with a
; structure, which may be either a number (representing a simple weight) or
; another mobile:

(define (make-branch length structure)
  (list length structure))

; a.  Write the corresponding selectors left-branch and right-branch, which
; return the branches of a mobile, and branch-length and branch-structure, which
; return the components of a branch.

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))

(define (branch-length m) (car m))
(define (branch-structure m) (car (cdr m)))  

; b.  Using your selectors, define a procedure total-weight that returns the
; total weight of a mobile.

(define (total-weight m)
  (if (not (pair?  m)) ; Verifico si m no es un mobile
      m ; Si no lo es, debe ser un peso simple
      (let ((mobile? pair?)
            (weight1 (branch-structure (left-branch m)))
            (weight2 (branch-structure (right-branch m))))
        (if (or (mobile? weight1) (mobile? weight2)) 
            (+ (total-weight weight1) (total-weight weight2))
            (+ weight1 weight2)))))

; c.  A mobile is said to be balanced if the torque applied by its top-left
; branch is equal to that applied by its top-right branch (that is, if the
; length of the left rod multiplied by the weight hanging from that rod is equal
; to the corresponding product for the right side) and if each of the submobiles
; hanging off its branches is balanced. Design a predicate that tests whether a
; binary mobile is balanced.

(define (balanced? m)
  (let ((left (left-branch m))
        (right (right-branch m)))
    (define (torque branch)
      (* (branch-length branch) (if (pair? (branch-structure branch))
                                    (total-weight (branch-structure branch))
                                    (branch-structure branch))))
    
    (and (= (torque left) (torque right))
         (if (pair? (branch-structure left))
             (balanced? (branch-structure left))
             #t)
         (if (pair? (branch-structure right))
             (balanced? (branch-structure right))
             #t))))
    
; d.  Suppose we change the representation of mobiles so that the constructors
; are  

(define (make-mobile2 left right)
  (cons left right))
(define (make-branch2 length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the new
; representation?

; Deberia actualizar los selectores nada mas, ya que el resto de los
; procedimientos solo utilizan estos, no utilizan make para nada
(define (left-branch2 m) (car m))
(define (right-branch2 m) (cdr m))
(define (branch-length2 m) (car m))
(define (branch-structure2 m) (cdr m))  

; Pruebas ---------------------------------------------------------------------
; Crear algunos ejemplos de móviles para probar total-weight.
(define weight1 5)  ; Peso simple
(define weight2 3)  ; Peso simple
(define weight3 2)  ; Peso simple

; Crear ramas simples.
(define branch1 (make-branch 3 weight1))  ; Rama de longitud 3 con peso 5
(define branch2 (make-branch 5 weight2))  ; Rama de longitud 5 con peso 3
(define branch3 (make-branch 6 weight3))  ; Rama de longitud 6 con peso 2

; Crear móviles complejos.
(define mobile1 (make-mobile branch1 branch2))  ; Móvil con dos ramas simples
(define mobile2 (make-mobile branch2 branch3))  ; Otro móvil con ramas simples

; Crear ramas complejas.
(define branch-comp1 (make-branch 1 mobile1))  ; Rama de longitud 1 con peso de mobile1
(define branch-comp2 (make-branch 5 mobile2))  ; Rama de longitud 5 con peso de mobile2

; Crear un móvil que contiene otros móviles.
(define complex-mobile1 (make-mobile branch-comp1 branch-comp2))
(define complex-mobile2 (make-mobile branch-comp1 branch2))

; Probar total-weight en diferentes móviles.
(display (total-weight mobile1))       ; Debería mostrar 8 (5 + 3)
(newline)
(display (total-weight mobile2))       ; Debería mostrar 5 (3 + 2)
(newline)
(display (total-weight complex-mobile1)) ; Debería mostrar 13 (8 de mobile1 + 5 de mobile2)
(newline)
(display (total-weight complex-mobile2)) ; Debería mostrar 11 (8 de mobile1 + 3 de branch2)
(newline)
(newline)
; Probar balanced? en diferentes móviles.
(display (balanced? (make-mobile branch-comp1 branch-comp2))) ; Debería mostrar false
(newline)
(display (balanced? (make-mobile branch2 branch-comp1))) ; Debería mostrar false
(newline)
(display (balanced? (make-mobile branch-comp1 branch-comp1))) ; Debería mostrar true
(newline)