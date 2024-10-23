#lang sicp
; ------------------------------- Exercise 2.33 --------------------------------   
; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:

; (define (map p sequence)
;   (accumulate (lambda (x y) <??>) nil sequence))
; (define (append seq1 seq2)
;   (accumulate cons <??> <??>))
; (define (length sequence)
;   (accumulate <??> 0 sequence))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Codigo viejo -----------------------------------------------------------------
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Pruebas ----------------------------------------------------------------------
(define lista1 (list 1 2 3 4))
(define lista2 (list 5 6 7 8))

(map (lambda (x) (* x 10)) lista1)
(append lista1 lista2)
(length lista2)