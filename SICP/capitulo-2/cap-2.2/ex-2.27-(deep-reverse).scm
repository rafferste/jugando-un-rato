#lang sicp
; ------------------------------- Exercise 2.27 --------------------------------   
; Modify your reverse procedure of exercise 2.18 to produce a deep-reverse
; procedure that takes a list as argument and returns as its value the list with
; its elements reversed and with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))
; x
; ((1 2) (3 4))

; (reverse x)
; ((3 4) (1 2))

; (deep-reverse x)
; ((4 3) (2 1))

(define (reverse items)
  (define (iter original reversed)
    (if (null? original)
        reversed
        (iter (cdr original) (cons (car original) reversed))))
  (iter items '()))

(define (deep-reverse items)
  (cond ((null? items) '())  ; Caso base: si la lista está vacía
        ((list? (car items))  ; Si el primer elemento es una lista
         (append (deep-reverse (cdr items))  ; Primero reversar el cdr
                 (list (deep-reverse (car items)))))  ; Luego reversar el car
        (else (append (deep-reverse (cdr items))  ; Si no es una lista
                      (list (car items))))))  ; append para mantener el orden 