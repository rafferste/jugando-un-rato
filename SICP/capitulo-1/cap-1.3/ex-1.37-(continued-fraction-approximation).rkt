#lang sicp
; ------------------------------- Exercise 1.36 --------------------------------   
; a. An infinite continued fraction is an expression of the form

; f = N1/(D1+N2/(D2+N3/(D3+...)))

; As an example, one can show that the infinite continued fraction expansion
; with the Ni and the Di all equal to 1 produces 1/(phi), where (phi) is the
; golden ratio (described in section 1.2.2). One way to approximate an infinite
; continued fraction is to truncate the expansion after a given number of terms.
; Such a truncation -- a so-called k-term finite continued fraction -- has the
; form

; N1/(D1+N2/(...+NK/DK))

; Suppose that n and d are procedures of one argument (the term index i) that
; return the Ni and Di of the terms of the continued fraction. Define a
; procedure cont-frac such that evaluating (cont-frac n d k) computes the value
; of the k-term finite continued fraction. Check your procedure by approximating
; 1/(phi) using

; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)

; for successive values of k. How large must you make k in order to get an
; approximation that is accurate to 4 decimal places? (0.6180) --> k=11

(define (cont-frac n d k)
  (define (recursiv i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recursiv (+ i 1))))))
  (recursiv 1))

; b. If your cont-frac procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

; Pruebas
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)