#lang sicp
; ------------------------------- Exercise 2.20 --------------------------------   
; The procedures +, *, and list take arbitrary numbers of arguments. One way to
; define such procedures is to use define with dotted-tail notation. In a
; procedure definition, a parameter list that has a dot before the last
; parameter name indicates that, when the procedure is called, the initial
; parameters (if any) will have as values the initial arguments, as usual, but
; the final parameter's value will be a list of any remaining arguments. For
; instance, given the definition

; (define (f x y . z) <body>)

; Use this notation to write a procedure same-parity that takes one or more
; integers and returns a list of all the arguments that have the same even-odd
; parity as the first argument. For example,

; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; (same-parity 2 3 4 5 6 7)
; (2 4 6)

(define (same-parity first . rest)
  (if (par? first)
      (cons first (retornar-pares rest))
      (cons first(retornar-impares rest))))

(define (par? n)
  (= (remainder n 2) 0))

(define (retornar-pares items)
  (cond ((null? items) '())
        ((par? (car items)) (cons (car items) (retornar-pares (cdr items))))
        (else (retornar-pares (cdr items)))))

(define (retornar-impares items)
  (cond ((null? items) '())
        ((not (par? (car items))) (cons (car items) (retornar-impares (cdr items))))
        (else (retornar-impares (cdr items)))))
  
