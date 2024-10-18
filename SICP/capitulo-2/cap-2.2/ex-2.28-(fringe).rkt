#lang sicp
; ------------------------------- Exercise 2.28 --------------------------------   
; Write a procedure fringe that takes as argument a tree (represented as a list)
; and returns a list whose elements are all the leaves of the tree arranged in
; left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))

; (fringe x)
; (1 2 3 4)

; (fringe (list x x))
; (1 2 3 4 1 2 3 4)

(define (fringe tree)
  ; Caso base
  (cond ((null? tree) nil)
        ; Cuando el car de tree es una lista...
        ((list? (car tree))
         ; aplica append a ambas partes de forma recursiva
         (append (fringe (car tree)) (fringe (cdr tree))))
        ; cuando es un elemento individual simplemento lo uno y sigo 
        (else (append (list (car tree)) (fringe (cdr tree)))))) 