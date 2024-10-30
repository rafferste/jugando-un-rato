#lang sicp
; ------------------------------- Exercise 2.47 --------------------------------
; Here are two possible constructors for frames:

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; For each constructor supply the appropriate selectors to produce an
; implementation for frames.

(define (origin-frame1 frame) (car frame))
(define (edge1-frame1 frame) (cadr frame))
(define (edge2-frame1 frame) (caddr frame))

(define (origin-frame2 frame) (car frame))
(define (edge1-frame2 frame) (cadr frame))
(define (edge2-frame2 frame) (cddr frame))

; Pruebas --------------------------------------------------------------------
(define frame1 (make-frame1 1 2 3))
(define frame2 (make-frame2 4 5 6))

(origin-frame1 frame1)
(edge1-frame1 frame1)
(edge2-frame1 frame1)

(origin-frame2 frame2)
(edge1-frame2 frame2)
(edge2-frame2 frame2)