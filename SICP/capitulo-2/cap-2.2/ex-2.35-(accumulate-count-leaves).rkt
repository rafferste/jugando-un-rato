#lang sicp
; ------------------------------- Exercise 2.35 --------------------------------   
; Redefine count-leaves from section 2.2.2 as an accumulation:

; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))

; Codigo viejo -----------------------------------------------------------------
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Pruebas ----------------------------------------------------------------------
(define tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)
        3
        (list 23 76)))

(count-leaves tree)