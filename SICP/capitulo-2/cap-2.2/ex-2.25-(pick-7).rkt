#lang sicp
; ------------------------------- Exercise 2.25 --------------------------------   
; Give combinations of cars and cdrs that will pick 7 from each of the following
; lists:

; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))

(define A (list 1 3 (list 5 7) 9))
(define B (list (list 7)))
(define C (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
A
B
C

(newline)

(cdr (car (cdr (cdr A))))
(cdaddr A)
(car B)
(cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr C)))))))))))
