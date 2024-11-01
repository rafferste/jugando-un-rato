#lang sicp
; ------------------------------- Exercise 2.52 --------------------------------
; Make changes to the square limit of wave shown in figure 2.9 by working at
; each of the levels described above. In particular:

; a.  Add some segments to the primitive wave painter of exercise  2.49 (to add
; a smile, for example).

; The wave painter.
(define wave
  (lambda (frame)
    (segments->painter
     (let ((p1 (make-vect 0 14))
           (p2 (make-vect 3 10))
           (p3 (make-vect 5 11))
           (p4 (make-vect 7 11))
           (p5 (make-vect 6 14))
           (p6 (make-vect 7 16))
           (p7 (make-vect 9 16))
           (p8 (make-vect 10 14))
           (p9 (make-vect 9 11))
           (p10 (make-vect 12 11))
           (p11 (make-vect 16 6))
           (p12 (make-vect 0 11))
           (p13 (make-vect 3 7))
           (p14 (make-vect 5 10))
           (p15 (make-vect 6 8))
           (p16 (make-vect 4 0))
           (p17 (make-vect 6 0))
           (p18 (make-vect 8 5))
           (p19 (make-vect 10 0))
           (p20 (make-vect 12 0))
           (p21 (make-vect 9 8))
           (p22 (make-vect 16 4))
           (p1-smile (make-vect 7 13)) ; Nuevo
           (p2-smile (make-vect 8 12)) ; Nuevo
           (p3-smile (make-vect 9 13))) ; Nuevo
       (let ((A (make-segment p1 p2))
             (B (make-segment p2 p3))
             (C (make-segment p3 p4))
             (D (make-segment p4 p5))
             (E (make-segment p5 p6))
             (F (make-segment p7 p8))
             (G (make-segment p8 p9))
             (GH (make-segment p9 p10))
             (H (make-segment p10 p11))
             (I (make-segment p12 p13))
             (J (make-segment p13 p14))
             (K (make-segment p14 p15))
             (L (make-segment p15 p16))
             (M (make-segment p17 p18))
             (N (make-segment p18 p19))
             (Ñ (make-segment p20 p21))
             (O (make-segment p21 p22))
             (A-smile (make-segment p1-smile p2-smile)) ; Nuevo
             (B-smile (make-setment p2-smile p3-smile))) ; Nuevo
         (let ((head (list D E F G A-smile B-smile)) ; Nuevo
               (left-arm (list A B C I J K))
               (right-arm (list GH H O))
               (left-leg (list L M))
               (right-leg (list N Ñ)))
           (append head left-arm right-arm left-leg right-leg)))))
    frame))

; b.  Change the pattern constructed by corner-split (for example, by using only
; one copy of the up-split and right-split images instead of two).

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; c.  Modify the version of square-limit that uses square-of-four so as to
; assemble the corners in a different pattern. (For example, you might make the
; big Mr. Rogers look outward from each corner of the square.)

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))